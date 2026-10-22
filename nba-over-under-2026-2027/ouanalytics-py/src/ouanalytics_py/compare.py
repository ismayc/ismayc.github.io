"""Recompute a golden case from its inputs and compare with R's outputs.

Rules: the same column names in the same order, the same row count, numbers
equal within 1e-9 (relative and absolute), NA equal to NA, and exact
equality for everything else. Seeding rows must match in order; pool tables
are compared after sorting both sides by every column, because the row order
of R's grouped tables is not part of the contract.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path

import polars as pl

from . import io
from .seeding import compute_playoff_seeding
from .standings import compute_pool

TOL = 1e-9
MAX_SHOWN = 5


@dataclass
class Result:
    """One line of the report: a table or a check, ok or not."""

    name: str
    ok: bool
    details: list[str] = field(default_factory=list)

    def lines(self, prefix: str = "") -> list[str]:
        if self.ok:
            return [f"{prefix}{self.name}: ok"]
        return [f"{prefix}{self.name}: FAIL", *(f"    {d}" for d in self.details)]


def compute_case(case_dir: Path) -> dict[str, pl.DataFrame]:
    """Every output table of a case, recomputed in Python from its inputs."""
    inputs = io.read_inputs(case_dir)
    if io.case_kind(case_dir) == "seeding":
        return {
            "seeding": compute_playoff_seeding(
                inputs["standings"], inputs["scores_tidy"], inputs["meta"]
            )
        }
    params = io.read_params(inputs["params"])
    pool = compute_pool(
        inputs["game_results_raw"],
        inputs["scores"],
        inputs["projections"],
        inputs["picks"],
        inputs["standings"],
        **params,
    )
    rankings = pool["current_rankings"]
    pool["current_rankings"] = pl.DataFrame(
        {"rank": range(1, len(rankings) + 1), "Player": rankings},
        schema={"rank": pl.Int64, "Player": pl.String},
    )
    return {name: pool[name] for name in io.POOL_OUTPUTS}


def _comparable(computed: pl.DataFrame, expected: pl.DataFrame) -> tuple[pl.DataFrame, pl.DataFrame, list[str]]:
    """Both tables in the same types. Numbers become Float64 (NaN as null,
    which is how R writes it); booleans become TRUE/FALSE text; dates
    YYYY-MM-DD text; everything else text. Raises ValueError when an R
    column will not parse as a number."""
    numeric: list[str] = []
    py_cols, r_cols = [], []
    for name, dtype in computed.schema.items():
        col = pl.col(name)
        if dtype.is_numeric():
            numeric.append(name)
            py_cols.append(col.cast(pl.Float64).fill_nan(None))
            parsed = expected[name].cast(pl.Float64, strict=False)
            bad = expected[name].is_not_null() & parsed.is_null()
            if bad.any():
                raise ValueError(
                    f"R column {name!r} is not numeric: {expected[name].filter(bad)[0]!r}"
                )
            r_cols.append(parsed.fill_nan(None))
        elif dtype == pl.Boolean:
            py_cols.append(
                pl.when(col).then(pl.lit("TRUE")).when(~col).then(pl.lit("FALSE")).alias(name)
            )
            r_cols.append(expected[name])
        elif dtype == pl.Date:
            py_cols.append(col.dt.to_string("%Y-%m-%d"))
            r_cols.append(expected[name])
        else:
            py_cols.append(col.cast(pl.String))
            r_cols.append(expected[name])
    return computed.select(py_cols), pl.DataFrame(r_cols), numeric


def _show(value) -> str:
    return repr(value) if value is not None else "NA"


def compare_table(
    name: str, computed: pl.DataFrame, expected: pl.DataFrame | None, ordered: bool
) -> Result:
    """Compare one recomputed table with R's."""
    if expected is None:
        return Result(name, False, ["R output file is missing"])
    if computed.columns != expected.columns:
        missing = [c for c in expected.columns if c not in computed.columns]
        extra = [c for c in computed.columns if c not in expected.columns]
        return Result(
            name,
            False,
            [
                f"columns differ: R has {expected.columns}",
                f"Python has {computed.columns}",
                f"only in R: {missing}; only in Python: {extra}",
            ],
        )
    if computed.height != expected.height:
        return Result(name, False, [f"row count differs: Python {computed.height}, R {expected.height}"])
    try:
        py, r, numeric = _comparable(computed, expected)
    except ValueError as err:
        return Result(name, False, [str(err)])
    if not ordered:
        py = py.sort(py.columns, nulls_last=True)
        r = r.sort(r.columns, nulls_last=True)

    a, b = pl.col("a"), pl.col("b")
    close = (a - b).abs() <= pl.max_horizontal(pl.lit(TOL), pl.max_horizontal(a.abs(), b.abs()) * TOL)
    same = {}
    for c in py.columns:
        eq = a.eq_missing(b)
        if c in numeric:
            eq = eq | close.fill_null(False)
        same[c] = pl.DataFrame({"a": py[c], "b": r[c]}).select(eq).to_series()
    diff_rows = pl.DataFrame(same).select(~pl.all_horizontal(pl.all())).to_series()
    n_bad = int(diff_rows.sum())
    if n_bad == 0:
        return Result(name, True)

    details = [f"{n_bad} of {py.height} rows differ (first {min(n_bad, MAX_SHOWN)} shown):"]
    for i in diff_rows.arg_true().head(MAX_SHOWN).to_list():
        cols = [c for c in py.columns if not same[c][i]]
        parts = [f"{c}: Python={_show(py[c][i])} R={_show(r[c][i])}" for c in cols]
        details.append(f"row {i + 1}: " + "; ".join(parts))
    return Result(name, False, details)


def compare_case(case_dir: Path) -> list[Result]:
    """Recompute a case and compare every output table with R's."""
    computed = compute_case(case_dir)
    expected = io.read_outputs(case_dir)
    ordered = io.case_kind(case_dir) == "seeding"
    return [compare_table(n, computed[n], expected[n], ordered) for n in expected]
