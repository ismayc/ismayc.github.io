"""Read a golden case folder (written by ouanalytics/R/export.R) into polars.

Layout: ``<case>/inputs/*.csv`` and ``<case>/outputs/*.csv``. NA is an empty
field, dates are YYYY-MM-DD, factors are text. Nothing here writes a file.
"""

from __future__ import annotations

import datetime as dt
from pathlib import Path

import polars as pl

POOL_INPUTS = ("game_results_raw", "scores", "projections", "picks", "standings", "params")
SEEDING_INPUTS = ("meta", "standings", "scores_tidy")

POOL_OUTPUTS = (
    "game_results",
    "join_with_projections",
    "standings_grid",
    "scored_picks",
    "changes_in_player_projections",
    "rows_today",
    "projected_score",
    "player_projections_by_team",
    "standings_with_projected",
    "wins_needed",
    "current_rankings",
)
SEEDING_OUTPUTS = ("seeding",)

# Known column types. Columns not named here are inferred from the whole file.
# A header-only file (no games yet) still gets usable types this way.
_STR = pl.String
_F64 = pl.Float64
_DATE = pl.Date
INPUT_SCHEMAS: dict[str, dict[str, pl.DataType]] = {
    "game_results_raw": {
        "slugSeason": _STR,
        "dateGame": _DATE,
        "numberGameTeamSeason": pl.Int64,
        "nameTeam": _STR,
        "slugMatchup": _STR,
        "slugTeam": _STR,
        "slugOpponent": _STR,
        "slugTeamLoser": _STR,
    },
    "scores": {
        "game_id": _STR,
        "game_date": _DATE,
        "away_team": _STR,
        "home_team": _STR,
        "slug_away_team": _STR,
        "slug_home_team": _STR,
        "away_score": _F64,
        "home_score": _F64,
    },
    "projections": {"team": _STR, "win_projection": _F64, "percentage_projection": _F64},
    "picks": {"team": _STR, "player": _STR, "wage": _F64, "choice": _STR},
    "standings": {
        "team_name": _STR,
        "conference": _STR,
        "division": _STR,
        "abbreviation": _STR,
        "wins": _F64,
        "losses": _F64,
        "Winning Pct": _F64,
        "Games Back": _STR,
    },
    "params": {"name": _STR, "value": _STR},
    "meta": {"team": _STR, "conference": _STR, "division": _STR, "abbreviation": _STR},
    "scores_tidy": {
        "team_slug": _STR,
        "opponent_slug": _STR,
        "score": _F64,
        "opponent_score": _F64,
    },
}


def _header(path: Path) -> list[str]:
    return pl.read_csv(path, n_rows=0).columns


def read_input(path: Path, name: str) -> pl.DataFrame:
    """Read one input CSV with the known types for its columns."""
    known = INPUT_SCHEMAS.get(name, {})
    cols = _header(path)
    overrides = {c: t for c, t in known.items() if c in cols}
    return pl.read_csv(path, schema_overrides=overrides, infer_schema_length=None)


def read_output(path: Path) -> pl.DataFrame:
    """Read an R output table with every column as text; compare.py types it."""
    return pl.read_csv(path, infer_schema_length=0)


def case_kind(case_dir: Path) -> str:
    """'pool' or 'seeding', from which input files exist."""
    inputs = Path(case_dir) / "inputs"
    if all((inputs / f"{n}.csv").exists() for n in POOL_INPUTS):
        return "pool"
    if all((inputs / f"{n}.csv").exists() for n in SEEDING_INPUTS):
        return "seeding"
    raise ValueError(f"{case_dir}: inputs/ holds neither a pool nor a seeding case")


def read_inputs(case_dir: Path) -> dict[str, pl.DataFrame]:
    kind = case_kind(case_dir)
    names = POOL_INPUTS if kind == "pool" else SEEDING_INPUTS
    inputs = Path(case_dir) / "inputs"
    return {n: read_input(inputs / f"{n}.csv", n) for n in names}


def read_params(params: pl.DataFrame) -> dict[str, object]:
    """params.csv (name,value) as typed values: three dates and num_games."""
    raw = dict(zip(params["name"].to_list(), params["value"].to_list(), strict=True))
    return {
        "yesterday": dt.date.fromisoformat(raw["yesterday"]),
        "today": dt.date.fromisoformat(raw["today"]),
        "season_start": dt.date.fromisoformat(raw["season_start"]),
        "num_games": float(raw["num_games"]),
    }


def read_outputs(case_dir: Path) -> dict[str, pl.DataFrame | None]:
    """Each expected output table, or None when its file is missing."""
    names = POOL_OUTPUTS if case_kind(case_dir) == "pool" else SEEDING_OUTPUTS
    outputs = Path(case_dir) / "outputs"
    return {
        n: (read_output(outputs / f"{n}.csv") if (outputs / f"{n}.csv").exists() else None)
        for n in names
    }
