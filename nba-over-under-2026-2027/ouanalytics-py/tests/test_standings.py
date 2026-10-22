"""Edge cases of the pool calculations that the golden cases do not reach.

Expected values were computed by R from ouanalytics/R/standings_logic.R on
October 22, 2026 (R 4.6.1) with the same inputs, for example:

    sp <- tibble(Date = d, Player = "P1", Team = c("over_done", ...), ...)
    add_outcome_columns(sp, 82); build_wins_needed(o, d, 82)
"""

from __future__ import annotations

import datetime as dt
import math

import polars as pl
import pytest

from ouanalytics_py.standings import (
    add_changes,
    add_outcome_columns,
    add_projected_to_standings,
    build_wins_needed,
    classify_over_under,
    compute_game_results,
    r_round,
    r_str,
    rank_players,
    rows_for_day,
    score_picks,
    tidy_scores,
)

D = dt.date(2027, 4, 12)
NA = None


def test_r_round_matches_r_on_ties_and_specials():
    # R 4.6.1: round(c(3.125, 2.675, -3.125, 1429/100*1, 0.285, 0, Inf, NaN, 1e15+0.5), 2)
    x = pl.DataFrame({"x": [3.125, 2.675, -3.125, 14.285714285714286, 0.285, 0.0, math.inf, None, 1e15 + 0.5]})
    got = x.select(r_round(pl.col("x")))["x"].to_list()
    assert got[:6] == [3.12, 2.67, -3.12, 14.29, 0.28, 0.0]
    assert got[6] == math.inf and got[7] is None and got[8] == 1e15 + 0.5


def test_r_round_differs_from_polars_round_where_r_does():
    # 14.285714285714285 rounds to the double 14.29 in R; polars' own round
    # path via a literal division gives 14.290000000000001.
    x = pl.DataFrame({"x": [14.285714285714285]})
    assert x.select(r_round(pl.col("x"))).item() == 14.29


def test_r_str_prints_like_paste0():
    x = pl.DataFrame({"x": [95.0, 95.5, None, math.inf]})
    assert x.select(r_str(pl.col("x")))["x"].to_list() == ["95", "95.5", "NA", "inf"]


def test_classify_over_under_missing_is_push():
    # R: classify_over_under(c(50, NA, 40, 50, NaN), c(40, 50, 50, 50, 50))
    df = pl.DataFrame({"a": [50.0, None, 40.0, 50.0, math.nan], "b": [40.0, 50, 50, 50, 50]})
    got = df.select(classify_over_under(pl.col("a"), pl.col("b")).alias("ou"))["ou"].to_list()
    assert got == ["OVER", "PUSH", "UNDER", "PUSH", "PUSH"]


def _scored():
    return pl.DataFrame(
        {
            "Date": [D] * 7,
            "Player": ["P1"] * 7,
            "Team": ["over_done", "under_done", "season_over_short", "season_over_level", "no_games", "open", "no_status"],
            "current_wins": [50.0, 10, 41, 41, 0, 30, 0],
            "current_losses": [10.0, 50, 41, 41, 0, 30, 0],
            "Current Record": ["50-10", "10-50", "41-41", "41-41", "0-0", "30-30", "0-0"],
            "Current Win %": [83.33, 16.67, 50, 50, None, 50, None],
            "Vegas Win Projection %": [50.0, 50, 60, 50, 53.05, 60, 50],
            "Current Over/Under": ["OVER", "UNDER", "UNDER", "PUSH", None, "UNDER", None],
            "Over/Under Pick": ["OVER"] * 7,
            "Win % - Vegas": [0.0] * 7,
            "current_projected_points": [15.0, -14, -13, 0, None, -12, None],
        }
    )


def test_add_outcome_columns_settled_and_finished_seasons():
    out = add_outcome_columns(_scored(), 82.0)
    assert out["Outcome Determined"].to_list() == [
        "OVER", "UNDER", "UNDER", "OVER", "not yet", "not yet", "not yet"
    ]
    assert out["Wins To Go Over Vegas"].to_list() == [NA, NA, NA, NA, 44, 20, 41]
    assert out["Winning % In Remaining Games Needed"].to_list() == [NA, NA, NA, NA, 53.66, 90.91, 50]
    assert out["Losses To Go Under Vegas"].to_list() == [NA, NA, NA, NA, 38, 2, 41]
    assert out.columns[-2:] == ["Current Projected Points", "Outcome Determined"]


def test_build_wins_needed_from_r():
    out = build_wins_needed(add_outcome_columns(_scored(), 82.0), D, 82.0)
    assert out.columns == [
        "Date", "Team", "Outcome Determined", "Current Record", "Wins", "Losses",
        "Wins To Go Over Vegas", "Losses To Go Under Vegas", "Remaining Games",
    ]
    assert out["Wins"].to_list() == [50, 10, 41, 41, 0, 30, 0]
    assert out["Wins To Go Over Vegas"].to_list() == [NA, NA, NA, NA, 44, 20, 41]
    assert out["Losses To Go Under Vegas"].to_list() == [NA, NA, NA, NA, 38, 2, 41]
    assert out["Remaining Games"].to_list() == [22, 22, 0, 0, 82, 22, 82]


def test_build_wins_needed_blanks_on_its_own_rules():
    # Values that add_outcome_columns would not produce, to reach each rule.
    ppbt = pl.DataFrame(
        {
            "Date": [D] * 3,
            "Team": ["a", "b", "c"],
            "Outcome Determined": ["not yet"] * 3,
            "Current Record": ["1-1", "1-1", "1-1"],
            "Wins To Go Over Vegas": [0.0, 5.0, 5.0],
            "Winning % In Remaining Games Needed": [50.0, 101.0, 50.0],
            "Losses To Go Under Vegas": [3.0, 3.0, -1.0],
        }
    )
    out = build_wins_needed(ppbt, D, 82.0)
    assert out["Wins To Go Over Vegas"].to_list() == [NA, NA, 5.0]
    assert out["Losses To Go Under Vegas"].to_list() == [NA, NA, NA]


def test_add_projected_to_standings_no_games_left_and_no_games_played():
    standings = pl.DataFrame({"team_name": ["A", "B", "C"], "wins": [41, 0, 60], "losses": [41, 0, 10]})
    proj = pl.DataFrame(
        {"team": ["A", "B", "C"], "win_projection": [40.5, 41, 50.5], "percentage_projection": [50.0] * 3}
    )
    out = add_projected_to_standings(standings, proj, D, 82.0)
    assert out.columns == [
        "team_name", "wins", "losses", "date", "current_win_perc", "win_projection",
        "remaining_games", "wins_needed_for_over", "winning_perc_for_over", "differential",
    ]
    assert out["remaining_games"].to_list() == [0, 82, 12]
    assert out["wins_needed_for_over"].to_list() == [0, 41, -9]
    assert out["winning_perc_for_over"].to_list() == [NA, 50, -75]
    assert math.isnan(out["current_win_perc"][1])  # R: NaN, written as an empty field
    assert out["differential"][2] == pytest.approx(160.714285714286, abs=1e-12)
    assert out["differential"][0] is None


def test_score_picks_no_status_is_na_and_push_is_zero():
    grid = pl.DataFrame(
        {
            "team": ["A", "A", "A"],
            "date": [D, D + dt.timedelta(1), D + dt.timedelta(2)],
            "current_wins": [0.0, 1, 1],
            "current_losses": [0.0, 0, 1],
            "Current Record": ["0-0", "1-0", "1-1"],
            "Current Win %": [None, 100.0, 50],
            "Vegas Win Projection %": [50.0] * 3,
            "over_under": [None, "OVER", "PUSH"],
        }
    )
    picks = pl.DataFrame({"team": ["A", "A"], "player": ["P1", "P2"], "wage": [10, 9], "choice": ["OVER", "UNDER"]})
    sp = score_picks(grid, picks)
    assert sp["current_projected_points"].to_list() == [NA, NA, 10, -9, 0, 0]
    changes = add_changes(sp)
    # NA counts as 0: the first game shows +10 / -9, the PUSH shows -10 / +9.
    assert changes["Change in Points Since Yesterday"].to_list() == [10, -9, -10, 9]
    assert rows_for_day(changes, D + dt.timedelta(2)).height == 2


def test_rank_players_uses_the_latest_date_and_tie_breaks():
    ps = pl.DataFrame(
        {
            "Date": [D, D, D, D - dt.timedelta(1)],
            "Player": ["A", "B", "C", "Z"],
            "Projected Total Points": [10.0, 10, 12, 99],
            "Number Correct": [2, 3, 1, 9],
            "Number (Wage 15) Correct": [0, 0, 0, 9],
        }
    )
    assert rank_players(ps, D + dt.timedelta(1)) == ["C", "B", "A"]


def test_la_names_are_spelled_out():
    raw = pl.DataFrame(
        {
            "dateGame": [D, D],
            "numberGameTeamSeason": [1, 2],
            "nameTeam": ["LA Lakers", "LA Lakers"],
            "slugTeam": ["LAL", "LAL"],
            "slugTeamLoser": ["X", "LAL"],
        }
    )
    gr = compute_game_results(raw)
    assert gr["Team Name"].to_list() == ["Los Angeles Lakers"] * 2
    assert gr["current_wins"].to_list() == [1, 1]
    assert gr["current_win_perc"].to_list() == [100, 50]
    scores = pl.DataFrame(
        {
            "game_id": ["g"], "game_date": [D], "away_team": ["LA Clippers"], "home_team": ["LA Lakers"],
            "slug_away_team": ["LAC"], "slug_home_team": ["LAL"], "away_score": [100.0], "home_score": [90.0],
        }
    )
    st = tidy_scores(scores)
    # Only the Clippers are renamed in scores, as in R.
    assert st["team_name"].to_list() == ["Los Angeles Clippers", "LA Lakers"]
    assert st["opponent_score"].to_list() == [90, 100]
    assert st["rowname"].to_list() == [1, 2]
