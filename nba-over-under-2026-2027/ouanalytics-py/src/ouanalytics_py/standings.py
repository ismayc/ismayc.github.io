"""Pool calculations: a polars re-implementation of ouanalytics/R/standings_logic.R
and pool.R.

The R code is the specification. Rules worth naming because polars differs
from R by default:

* R's ``if_else(cond, NA, x)`` gives NA when ``cond`` is NA; polars'
  ``when(null)`` takes the ``otherwise`` branch. ``_na_if`` restores R's rule.
* R's ``NaN > 100`` is NA; polars says true. Ratios that can be 0/0 have NaN
  turned into null before any comparison (R writes NaN as an empty field too).
* R's ``round(x, 2)`` picks the nearer of the two 2-decimal neighbours of the
  double ``x`` and breaks exact ties to even. ``r_round`` does the same.
* Row order inside a team (for running totals and "the game before") is the
  input order, as in R, so every join keeps the left order.
"""

from __future__ import annotations

import datetime as dt

import polars as pl

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------


def _fdiv(x: pl.Expr, d: float) -> pl.Expr:
    """``x / d`` with IEEE division, as R does it.

    polars 2.0 divides a column by a scalar by multiplying with ``1/d``
    (1429 / 100 gives 14.290000000000001, R gives 14.29). Dividing by a
    column is exact, so ``d`` is broadcast as a non-scalar column. Used only
    in frame-level ``with_columns``/``select``.
    """
    return x / (pl.int_range(pl.len()).cast(pl.Float64) * 0.0 + d)


def r_round(x: pl.Expr, digits: int = 2) -> pl.Expr:
    """R (>= 4.0.0) ``round(x, digits)`` for digits > 0, as a polars expression.

    Mirrors src/nmath/fround.c: take the two neighbours ``floor(x*10^d)/10^d``
    and ``ceil(x*10^d)/10^d`` of ``|x|``, return the nearer one, and on an
    exact tie the one whose scaled value is even. Non-finite values, zero, and
    values with more than 15 significant digits requested pass through.
    """
    p10 = 10.0**digits
    x = x.cast(pl.Float64)
    ax = x.abs()
    x10 = ax * p10
    i10 = x10.floor()
    down = _fdiv(i10, p10)
    up = _fdiv(x10.ceil(), p10)
    du = up - ax
    dd = ax - down
    take_up = (du < dd) | ((du == dd) & ((i10 % 2) == 1))
    rounded = pl.when(take_up).then(up).otherwise(down) * x.sign()
    passthrough = ~x.is_finite() | (x == 0) | ((ax.log10() + digits) > 15)
    return pl.when(passthrough).then(x).otherwise(rounded)


def r_str(x: pl.Expr) -> pl.Expr:
    """A number as R's paste0 shows it: 95 (not 95.0), and NA when missing."""
    num = x.cast(pl.Float64)
    as_text = (
        pl.when(num.is_finite() & (num == num.floor()))
        .then(num.cast(pl.Int64, strict=False).cast(pl.String))
        .otherwise(num.cast(pl.String))
    )
    return as_text.fill_null("NA")


def _na_if(cond: pl.Expr, value: pl.Expr) -> pl.Expr:
    """R's ``if_else(cond, NA_real_, value)``: NA when cond is TRUE or NA."""
    return pl.when(cond.fill_null(True)).then(pl.lit(None, pl.Float64)).otherwise(value)


def _nan_to_null(x: pl.Expr) -> pl.Expr:
    return x.cast(pl.Float64).fill_nan(None)


# ---------------------------------------------------------------------------
# one function per R function
# ---------------------------------------------------------------------------


def compute_game_results(game_results_raw: pl.DataFrame) -> pl.DataFrame:
    """One row per team game with running wins, losses, and win percentage."""
    games = pl.col("current_wins") + pl.col("current_losses")
    return (
        game_results_raw.with_columns(
            win=(pl.col("slugTeam") != pl.col("slugTeamLoser")).cast(pl.Float64)
        )
        .with_columns(loss=1.0 - pl.col("win"))
        .with_columns(
            current_wins=pl.col("win").cum_sum().over("nameTeam"),
            current_losses=pl.col("loss").cum_sum().over("nameTeam"),
        )
        .with_columns(current_win_perc=pl.col("current_wins") / games * 100)
        .rename(
            {
                "numberGameTeamSeason": "Game Number",
                "nameTeam": "Team Name",
                "dateGame": "Game Date",
            }
        )
        .with_columns(
            pl.col("Team Name").str.replace_all("LA ", "Los Angeles ", literal=True)
        )
        .sort("Team Name", maintain_order=True)
    )


def tidy_scores(scores: pl.DataFrame) -> pl.DataFrame:
    """One row per team per game: the away row, then the home row, each with
    the other side's score as ``opponent_score``."""
    games = scores.with_row_index("_game")
    sides = []
    for location, us, them in (("away", "away", "home"), ("home", "home", "away")):
        sides.append(
            games.with_columns(
                location=pl.lit(location),
                score=pl.col(f"{us}_score"),
                team_slug=pl.col(f"slug_{us}_team"),
                opponent_slug=pl.col(f"slug_{them}_team"),
                team_name=pl.col(f"{us}_team"),
                opponent_score=pl.col(f"{them}_score"),
            )
        )
    return (
        pl.concat(sides)
        .sort("_game", "location")
        .drop("_game", "away_score", "home_score")
        .with_columns(
            pl.col("team_name").str.replace_all(
                "LA Clippers", "Los Angeles Clippers", literal=True
            )
        )
        .with_row_index("rowname", offset=1)
        .with_columns(pl.col("rowname").cast(pl.Float64))
    )


def add_game_scores(game_results: pl.DataFrame, scores_tidy: pl.DataFrame) -> pl.DataFrame:
    """Attach each game's final score and a readable result string."""
    joined = game_results.join(
        scores_tidy.select("game_id", "game_date", "team_name", "score", "opponent_score"),
        left_on=["Game Date", "Team Name"],
        right_on=["game_date", "team_name"],
        how="left",
        maintain_order="left",
    )
    cols = [c for c in joined.columns if c != "game_id"]
    at = cols.index("Game Date") + 1
    cols = cols[:at] + ["game_id"] + cols[at:]
    final_score = pl.concat_str(r_str(pl.col("score")), pl.lit("-"), r_str(pl.col("opponent_score")))
    return (
        joined.select(cols)
        .with_columns(final_score=final_score)
        .with_columns(
            pl.concat_str(
                r_str(pl.col("current_wins")), pl.lit("-"), r_str(pl.col("current_losses"))
            ).alias("Current Record")
        )
        .with_columns(
            pl.concat_str(
                pl.lit("\nGame Number: "),
                r_str(pl.col("Game Number")),
                pl.lit("\nGame Date: "),
                pl.col("Game Date").dt.to_string("%Y-%m-%d").fill_null("NA"),
                pl.lit("\nGame Matchup: "),
                pl.col("slugMatchup").fill_null("NA"),
                pl.lit("\nFinal Score: "),
                pl.col("final_score"),
            ).alias("Game Result")
        )
    )


def classify_over_under(current_win_pct: pl.Expr, vegas_pct: pl.Expr) -> pl.Expr:
    """OVER above the line, UNDER below, PUSH when level or either is missing."""
    cur = _nan_to_null(current_win_pct)
    line = _nan_to_null(vegas_pct)
    return (
        pl.when(cur > line)
        .then(pl.lit("OVER"))
        .when(cur < line)
        .then(pl.lit("UNDER"))
        .otherwise(pl.lit("PUSH"))
    )


def add_over_under(
    updated_schedule: pl.DataFrame, projections: pl.DataFrame, yesterday: dt.date
) -> pl.DataFrame:
    """Each game with the team's status against its line, and whether the
    status flipped from the team's game before."""
    ou = pl.col("over_under")
    return (
        updated_schedule.join(
            projections, left_on="Team Name", right_on="team", how="inner", maintain_order="left"
        )
        .with_columns(
            r_round(pl.col("current_win_perc")).alias("Current Win %"),
            r_round(pl.col("percentage_projection")).alias("Vegas Win Projection %"),
        )
        .with_columns(
            over_under=classify_over_under(
                pl.col("Current Win %"), pl.col("Vegas Win Projection %")
            )
        )
        .with_columns(
            flipped=pl.when(ou.shift(1).over("Team Name") != ou).then(pl.lit(True)),
            flipped_today=pl.when(pl.col("Game Date") == yesterday).then(pl.lit(True)),
        )
    )


GRID_CARRIED = ["current_wins", "current_losses", "Current Record", "Current Win %", "over_under"]


def build_standings_grid(
    join_with_projections: pl.DataFrame, season_start: dt.date, yesterday: dt.date
) -> pl.DataFrame:
    """One row per team per day from season_start to yesterday. A team keeps its
    last status on days it does not play and has none before its first game."""
    teams = join_with_projections.select(pl.col("Team Name").unique().sort().alias("team"))
    days = pl.DataFrame({"date": pl.date_range(season_start, yesterday, "1d", eager=True)})
    games = join_with_projections.select(
        "Team Name",
        "Game Date",
        "current_wins",
        "current_losses",
        "Current Record",
        "Current Win %",
        "Vegas Win Projection %",
        "over_under",
    )
    return (
        teams.join(days, how="cross")
        .sort("team", "date")
        .join(
            games,
            left_on=["team", "date"],
            right_on=["Team Name", "Game Date"],
            how="left",
            maintain_order="left",
        )
        .with_columns(pl.col(GRID_CARRIED).forward_fill().over("team"))
        .with_columns(
            pl.col("Vegas Win Projection %").forward_fill().backward_fill().over("team")
        )
        .with_columns(
            pl.col("current_wins", "current_losses").fill_null(0.0),
            pl.col("Current Record").fill_null("0-0"),
        )
    )


def score_picks(standings_grid: pl.DataFrame, picks: pl.DataFrame) -> pl.DataFrame:
    """Each player's points for each team they picked, every day."""
    ou, pick, wage = pl.col("over_under"), pl.col("choice"), pl.col("wage").cast(pl.Float64)
    points = (
        pl.when(ou == "PUSH")
        .then(pl.lit(0.0))
        .when(ou == pick)
        .then(wage)
        .when(ou != pick)
        .then(-wage)
    )
    return (
        standings_grid.join(picks, on="team", how="inner", maintain_order="left")
        .with_columns(current_projected_points=points)
        .select(
            pl.col("date").alias("Date"),
            pl.col("player").alias("Player"),
            pl.col("team").alias("Team"),
            "current_wins",
            "current_losses",
            "Current Record",
            "Current Win %",
            "Vegas Win Projection %",
            pl.col("over_under").alias("Current Over/Under"),
            pl.col("choice").alias("Over/Under Pick"),
            r_round(pl.col("Current Win %") - pl.col("Vegas Win Projection %")).alias(
                "Win % - Vegas"
            ),
            "current_projected_points",
        )
    )


CHANGE_COLUMNS = [
    "Date",
    "Player",
    "Team",
    "Current Record",
    "Current Win %",
    "Vegas Win Projection %",
    "Change in Points Since Yesterday",
]


def add_changes(player_projections_by_team: pl.DataFrame) -> pl.DataFrame:
    """Rows where a pick's points moved from the day before; NA counts as 0."""
    pts = pl.col("current_projected_points")
    change = pts.fill_null(0.0) - pts.shift(1).over("Player", "Team").fill_null(0.0)
    return (
        player_projections_by_team.with_columns(change.alias("Change in Points Since Yesterday"))
        .sort("Date", "Team", "Player", maintain_order=True)
        .filter(pl.col("Change in Points Since Yesterday") != 0)
        .select(CHANGE_COLUMNS)
    )


def rows_for_day(changes: pl.DataFrame, day: dt.date) -> pl.DataFrame:
    return changes.filter(pl.col("Date") == day)


def projected_score_by_player(player_projections_by_team: pl.DataFrame) -> pl.DataFrame:
    """Each player's total and counts of correct picks, every day. Missing
    points (a team that has not played) count toward nothing."""
    pts = pl.col("current_projected_points")
    wage_counts = [
        (pts == w).sum().cast(pl.Int64).alias(f"Number (Wage {w}) Correct") for w in (15, 14, 13, 12)
    ]
    return (
        player_projections_by_team.group_by("Date", "Player")
        .agg(
            pts.sum().alias("Projected Total Points"),
            (pts > 0).sum().cast(pl.Int64).alias("Number Correct"),
            *wage_counts,
        )
        .sort("Date", "Player")
    )


def add_outcome_columns(player_projections_by_team: pl.DataFrame, num_games: float) -> pl.DataFrame:
    """Wins and losses each team still needs, and whether the outcome is settled."""
    line_wins = (_fdiv(pl.col("Vegas Win Projection %"), 100.0) * num_games).ceil()
    wins, losses = pl.col("current_wins"), pl.col("current_losses")
    w = pl.col("Wins To Go Over Vegas")
    pct = pl.col("Winning % In Remaining Games Needed")
    lo = pl.col("Losses To Go Under Vegas")
    settled = (lo < 0) | (pct > 100) | (w <= 0)
    df = (
        player_projections_by_team.with_columns((line_wins - wins).alias("Wins To Go Over Vegas"))
        .with_columns(
            _nan_to_null(r_round(w / (num_games - wins - losses) * 100)).alias(
                "Winning % In Remaining Games Needed"
            )
        )
        .with_columns(
            pl.when(w <= 0)
            .then(pl.lit("OVER"))
            .when(pct > 100)
            .then(pl.lit("UNDER"))
            .otherwise(pl.lit("not yet"))
            .alias("Outcome Determined"),
            (num_games - line_wins - losses).alias("Losses To Go Under Vegas"),
        )
    )
    # Applied one at a time, as in R: each uses the columns already blanked.
    for col in ("Winning % In Remaining Games Needed", "Wins To Go Over Vegas", "Losses To Go Under Vegas"):
        df = df.with_columns(_na_if(settled, pl.col(col)).alias(col))
    return df.select(
        "Date",
        "Player",
        "Team",
        "Current Record",
        "Current Win %",
        "Vegas Win Projection %",
        "Current Over/Under",
        "Over/Under Pick",
        "Win % - Vegas",
        "Wins To Go Over Vegas",
        "Winning % In Remaining Games Needed",
        "Losses To Go Under Vegas",
        pl.col("current_projected_points").alias("Current Projected Points"),
        "Outcome Determined",
    )


def rank_players(projected_score: pl.DataFrame, today: dt.date) -> list[str]:
    """Players on the date nearest today, best first. Ties keep the
    (Date, Player) order of projected_score."""
    days = (pl.lit(today) - pl.col("Date")).dt.total_days()
    return (
        projected_score.filter(days == days.min())
        .sort(
            "Projected Total Points",
            "Number Correct",
            "Number (Wage 15) Correct",
            descending=True,
            maintain_order=True,
        )["Player"]
        .to_list()
    )


def add_projected_to_standings(
    standings: pl.DataFrame, projections: pl.DataFrame, yesterday: dt.date, num_games: float
) -> pl.DataFrame:
    """Official standings with the wins each team needs for its OVER."""
    wins, losses = pl.col("wins"), pl.col("losses")
    remaining = pl.col("remaining_games")
    return (
        standings.with_columns(date=pl.lit(yesterday))
        .with_columns(wins.cast(pl.Float64), losses.cast(pl.Float64))
        .with_columns(current_win_perc=wins / (wins + losses) * 100)
        .join(projections, left_on="team_name", right_on="team", how="inner", maintain_order="left")
        .drop("percentage_projection")
        .with_columns(
            remaining_games=num_games - (wins + losses),
            wins_needed_for_over=pl.col("win_projection").cast(pl.Float64).ceil() - wins,
        )
        .with_columns(
            winning_perc_for_over=pl.when(remaining > 0).then(
                pl.col("wins_needed_for_over") / remaining * 100
            )
        )
        .with_columns(differential=pl.col("current_win_perc") - pl.col("winning_perc_for_over"))
    )


def build_wins_needed(
    player_projections_by_team: pl.DataFrame, yesterday: dt.date, num_games: float
) -> pl.DataFrame:
    """One row per team for yesterday: record, wins to go, games left."""
    w = pl.col("Wins To Go Over Vegas")
    lo = pl.col("Losses To Go Under Vegas")
    pct = pl.col("Winning % In Remaining Games Needed")
    record = pl.col("Current Record").str.split_exact("-", 1)
    return (
        player_projections_by_team.filter(pl.col("Date") == yesterday)
        .select(
            "Date",
            "Team",
            "Outcome Determined",
            "Current Record",
            "Wins To Go Over Vegas",
            "Winning % In Remaining Games Needed",
            "Losses To Go Under Vegas",
        )
        .unique(maintain_order=True)
        .with_columns(_na_if((w <= 0) | (pct > 100), w).alias("Wins To Go Over Vegas"))
        .with_columns(_na_if((lo < 0) | w.is_null(), lo).alias("Losses To Go Under Vegas"))
        .with_columns(
            Wins=record.struct.field("field_0").cast(pl.Int64),
            Losses=record.struct.field("field_1").cast(pl.Int64),
        )
        .with_columns((num_games - pl.col("Wins") - pl.col("Losses")).alias("Remaining Games"))
        .select(
            "Date",
            "Team",
            "Outcome Determined",
            "Current Record",
            "Wins",
            "Losses",
            "Wins To Go Over Vegas",
            "Losses To Go Under Vegas",
            "Remaining Games",
        )
    )


# ---------------------------------------------------------------------------
# pool.R
# ---------------------------------------------------------------------------


def compute_pool(
    game_results_raw: pl.DataFrame,
    scores: pl.DataFrame,
    projections: pl.DataFrame,
    picks: pl.DataFrame,
    standings: pl.DataFrame,
    yesterday: dt.date,
    today: dt.date,
    season_start: dt.date,
    num_games: float,
) -> dict[str, pl.DataFrame | list[str]]:
    """Every table the page uses, in the same order as R's compute_pool()."""
    game_results = compute_game_results(game_results_raw)
    scores_tidy = tidy_scores(scores)
    updated_schedule = add_game_scores(game_results, scores_tidy)
    join_with_projections = add_over_under(updated_schedule, projections, yesterday)
    standings_grid = build_standings_grid(join_with_projections, season_start, yesterday)
    scored_picks = score_picks(standings_grid, picks)
    changes = add_changes(scored_picks)
    projected_score = projected_score_by_player(scored_picks)
    player_projections_by_team = add_outcome_columns(scored_picks, num_games)
    return {
        "game_results": game_results,
        "scores_tidy": scores_tidy,
        "join_with_projections": join_with_projections,
        "standings_grid": standings_grid,
        "scored_picks": scored_picks,
        "changes_in_player_projections": changes,
        "rows_today": rows_for_day(changes, yesterday),
        "projected_score": projected_score,
        "player_projections_by_team": player_projections_by_team,
        "current_rankings": rank_players(projected_score, today),
        "standings_with_projected": add_projected_to_standings(
            standings, projections, yesterday, num_games
        ),
        "wins_needed": build_wins_needed(player_projections_by_team, yesterday, num_games),
    }
