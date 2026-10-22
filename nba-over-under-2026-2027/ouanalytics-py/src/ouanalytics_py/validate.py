"""Data checks that need no R: invariants of a case's inputs and of R's
outputs that follow from the code in ouanalytics/R.

Pool cases
  picks_unique         no (team, player) pair appears twice in picks
  games_add_up         in game_results, each team's final wins + losses
                       equals its number of game rows
  grid_complete        standings_grid has one row per team per day from
                       season_start to yesterday
  points_match_status  each scored pick's points are 0 for PUSH, +wage when
                       the status matches the pick, -wage when not, and NA
                       when the team has no status yet
  rows_today_date      every rows_today row is dated yesterday
Seeding cases
  seeds_contiguous     seeds are 1..n in order within each conference
  pct_non_increasing   Winning Pct never rises down the seeds
  seed_note_matches    seed_note is Playoff (<= 6), Play-In (<= 10), else ""
  teams_unique         each abbreviation is seeded once
"""

from __future__ import annotations

from pathlib import Path

import polars as pl

from . import io
from .compare import MAX_SHOWN, Result

F64 = pl.Float64


def _from_rows(name: str, bad: pl.DataFrame, what: str) -> Result:
    """A Result that fails when ``bad`` has rows, listing the first few."""
    if bad.height == 0:
        return Result(name, True)
    details = [f"{bad.height} {what} (first {min(bad.height, MAX_SHOWN)} shown):"]
    details += [str(row) for row in bad.head(MAX_SHOWN).iter_rows(named=True)]
    return Result(name, False, details)


def _skipped(name: str, table: str) -> Result:
    return Result(name, False, [f"cannot check: outputs/{table}.csv is missing"])


# ---------------------------------------------------------------------------
# pool
# ---------------------------------------------------------------------------


def picks_unique(picks: pl.DataFrame) -> Result:
    dup = picks.group_by("team", "player", maintain_order=True).len().filter(pl.col("len") > 1)
    return _from_rows("picks_unique", dup, "duplicated (team, player) pairs in picks")


def games_add_up(game_results: pl.DataFrame) -> Result:
    totals = (
        game_results.group_by("Team Name", maintain_order=True)
        .agg(
            games=pl.len(),
            wins_plus_losses=(pl.col("current_wins").cast(F64) + pl.col("current_losses").cast(F64)).max(),
        )
        .filter(pl.col("wins_plus_losses") != pl.col("games"))
    )
    return _from_rows("games_add_up", totals, "teams whose wins + losses differ from their games")


def grid_complete(standings_grid: pl.DataFrame, season_start, yesterday) -> Result:
    days = pl.date_range(season_start, yesterday, "1d", eager=True)
    per_team = (
        standings_grid.with_columns(pl.col("date").str.to_date())
        .group_by("team", maintain_order=True)
        .agg(
            rows=pl.len(),
            days=pl.col("date").n_unique(),
            first=pl.col("date").min(),
            last=pl.col("date").max(),
        )
        .filter(
            (pl.col("rows") != days.len())
            | (pl.col("days") != days.len())
            | (pl.col("first") != season_start)
            | (pl.col("last") != yesterday)
        )
    )
    return _from_rows(
        "grid_complete",
        per_team,
        f"teams without exactly one row per day {season_start} to {yesterday}",
    )


def points_match_status(scored_picks: pl.DataFrame, picks: pl.DataFrame) -> Result:
    status, pick = pl.col("Current Over/Under"), pl.col("Over/Under Pick")
    wage = pl.col("wage").cast(F64)
    expected = (
        pl.when(status.is_null())
        .then(pl.lit(None, F64))
        .when(status == "PUSH")
        .then(pl.lit(0.0))
        .when(status == pick)
        .then(wage)
        .otherwise(-wage)
    )
    checked = scored_picks.join(
        picks.select("team", "player", "wage"),
        left_on=["Team", "Player"],
        right_on=["team", "player"],
        how="left",
    ).with_columns(
        points=pl.col("current_projected_points").cast(F64), expected=expected
    )
    bad = checked.filter(
        ~pl.col("points").eq_missing(pl.col("expected")) | pl.col("wage").is_null()
    ).select("Date", "Player", "Team", "Current Over/Under", "Over/Under Pick", "wage", "points")
    return _from_rows("points_match_status", bad, "scored picks whose points do not follow from status, pick, and wage")


def rows_today_date(rows_today: pl.DataFrame, yesterday) -> Result:
    bad = rows_today.filter(pl.col("Date") != yesterday.isoformat())
    return _from_rows("rows_today_date", bad, f"rows_today rows not dated {yesterday}")


def validate_pool(case_dir: Path) -> list[Result]:
    inputs = io.read_inputs(case_dir)
    outputs = io.read_outputs(case_dir)
    params = io.read_params(inputs["params"])
    yesterday = params["yesterday"]
    checks = {
        "games_add_up": ("game_results", lambda t: games_add_up(t)),
        "grid_complete": (
            "standings_grid",
            lambda t: grid_complete(t, params["season_start"], yesterday),
        ),
        "points_match_status": ("scored_picks", lambda t: points_match_status(t, inputs["picks"])),
        "rows_today_date": ("rows_today", lambda t: rows_today_date(t, yesterday)),
    }
    results = [picks_unique(inputs["picks"])]
    for name, (table, check) in checks.items():
        frame = outputs[table]
        results.append(_skipped(name, table) if frame is None else check(frame))
    return results


# ---------------------------------------------------------------------------
# seeding
# ---------------------------------------------------------------------------


def _seeding_typed(seeding: pl.DataFrame) -> pl.DataFrame:
    return seeding.with_row_index("_row").with_columns(
        pl.col("seed").cast(pl.Int64), pl.col("Winning Pct").cast(F64)
    )


def seeds_contiguous(seeding: pl.DataFrame) -> Result:
    s = _seeding_typed(seeding).with_columns(
        expected_seed=pl.int_range(1, pl.len() + 1).over("conference")
    )
    bad = s.filter(pl.col("seed") != pl.col("expected_seed")).select(
        "conference", "seed", "expected_seed", "abbreviation"
    )
    return _from_rows("seeds_contiguous", bad, "rows out of 1..n seed order")


def pct_non_increasing(seeding: pl.DataFrame) -> Result:
    s = _seeding_typed(seeding).with_columns(
        above=pl.col("Winning Pct").shift(1).over("conference")
    )
    bad = s.filter(pl.col("Winning Pct") > pl.col("above")).select(
        "conference", "seed", "abbreviation", "Winning Pct", "above"
    )
    return _from_rows("pct_non_increasing", bad, "teams seeded below a team with a lower Winning Pct")


def seed_note_matches(seeding: pl.DataFrame) -> Result:
    seed = pl.col("seed")
    note = (
        pl.when(seed <= 6).then(pl.lit("Playoff")).when(seed <= 10).then(pl.lit("Play-In")).otherwise(pl.lit(""))
    )
    bad = (
        _seeding_typed(seeding)
        .with_columns(expected_note=note)
        .filter(~pl.col("seed_note").eq_missing(pl.col("expected_note")))
        .select("conference", "seed", "abbreviation", "seed_note", "expected_note")
    )
    return _from_rows("seed_note_matches", bad, "rows whose seed_note does not match the seed")


def teams_unique(seeding: pl.DataFrame) -> Result:
    dup = seeding.group_by("abbreviation", maintain_order=True).len().filter(pl.col("len") > 1)
    return _from_rows("teams_unique", dup, "abbreviations seeded more than once")


def validate_seeding(case_dir: Path) -> list[Result]:
    seeding = io.read_outputs(case_dir)["seeding"]
    names = ["seeds_contiguous", "pct_non_increasing", "seed_note_matches", "teams_unique"]
    if seeding is None:
        return [_skipped(n, "seeding") for n in names]
    return [seeds_contiguous(seeding), pct_non_increasing(seeding), seed_note_matches(seeding), teams_unique(seeding)]


def validate_case(case_dir: Path) -> list[Result]:
    if io.case_kind(case_dir) == "pool":
        return validate_pool(case_dir)
    return validate_seeding(case_dir)
