"""The comparison and the data checks must FAIL on broken data.

Each test copies a golden case to a temporary folder, breaks one thing, and
checks that ``ouanalytics-check`` exits 1 and says what is wrong.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
import pytest

from ouanalytics_py.cli import main
from ouanalytics_py.compare import Result, compare_table


def replace_once(path: Path, old: str, new: str) -> None:
    text = path.read_text()
    assert text.count(old) == 1, f"{old!r} found {text.count(old)} times in {path}"
    path.write_text(text.replace(old, new))


def run(case: Path, capsys) -> tuple[int, str]:
    code = main([str(case)])
    return code, capsys.readouterr().out


# --- pool: comparison ------------------------------------------------------


def test_changed_number_is_reported(copy_case, capsys):
    case = copy_case("fixture")
    replace_once(
        case / "outputs" / "scored_picks.csv",
        '2026-10-21,"P2","Los Angeles Clippers",1,0,"1-0",100,50,"OVER","OVER",50,13',
        '2026-10-21,"P2","Los Angeles Clippers",1,0,"1-0",100,50,"OVER","OVER",50,12',
    )
    code, out = run(case, capsys)
    assert code == 1
    assert "  scored_picks: FAIL" in out
    assert "1 of 8 rows differ" in out
    assert "current_projected_points: Python=13.0 R=12.0" in out
    # The data check sees the same error without Python's recomputation.
    assert "validate: points_match_status: FAIL" in out
    assert out.strip().endswith("DIFFERENCES FOUND")


def test_dropped_column_is_reported(copy_case, capsys):
    case = copy_case("oct21")
    path = case / "outputs" / "standings_grid.csv"
    pl.read_csv(path, infer_schema_length=0).drop("over_under").write_csv(path)
    code, out = run(case, capsys)
    assert code == 1
    assert "standings_grid: FAIL" in out
    assert "only in R: []; only in Python: ['over_under']" in out
    assert "  game_results: ok" in out  # other tables still compared


def test_duplicated_pick_is_reported(copy_case, capsys):
    case = copy_case("fixture")
    path = case / "inputs" / "picks.csv"
    path.write_text(path.read_text() + '"Alpha","P1",15,"OVER"\n')
    code, out = run(case, capsys)
    assert code == 1
    assert "validate: picks_unique: FAIL" in out
    assert "1 duplicated (team, player) pairs in picks" in out
    assert "{'team': 'Alpha', 'player': 'P1', 'len': 2}" in out
    assert "row count differs: Python 10, R 8" in out


def test_value_within_tolerance_passes_and_beyond_fails(copy_case, capsys):
    case = copy_case("fixture")
    path = case / "outputs" / "standings_with_projected.csv"
    replace_once(path, "50.6172839506173,-50.6172839506173", "50.6172839506174,-50.6172839506173")
    code, out = run(case, capsys)
    assert code == 0, out
    replace_once(path, "50.6172839506174,-50.6172839506173", "50.6172839506173,-50.6172849506173")
    code, out = run(case, capsys)
    assert code == 1
    assert "differential: Python=-50.617283950617285 R=-50.6172849506173" in out


def test_na_versus_value_is_a_difference(copy_case, capsys):
    case = copy_case("fixture")
    replace_once(
        case / "outputs" / "standings_grid.csv",
        '"Los Angeles Clippers",2026-10-20,0,0,"0-0",,50,',
        '"Los Angeles Clippers",2026-10-20,0,0,"0-0",0,50,',
    )
    code, out = run(case, capsys)
    assert code == 1
    assert "Current Win %: Python=NA R=0.0" in out


def test_non_numeric_r_value_is_reported(copy_case, capsys):
    case = copy_case("fixture")
    replace_once(case / "outputs" / "projected_score.csv", '2026-10-20,"P1",29,', '2026-10-20,"P1",abc,')
    code, out = run(case, capsys)
    assert code == 1
    assert "R column 'Projected Total Points' is not numeric: 'abc'" in out


def test_boolean_and_text_differences(copy_case, capsys):
    case = copy_case("fixture")
    path = case / "outputs" / "join_with_projections.csv"
    jwp = pl.read_csv(path, infer_schema_length=0)
    jwp.with_columns(
        pl.when(pl.col("Team Name") == "Bravo").then(pl.lit("TRUE")).otherwise(pl.col("flipped_today")).alias("flipped_today"),
        pl.when(pl.col("Team Name") == "Bravo").then(pl.lit("BRA x ALP")).otherwise(pl.col("slugMatchup")).alias("slugMatchup"),
    ).write_csv(path)
    code, out = run(case, capsys)
    assert code == 1
    assert "flipped_today: Python=NA R='TRUE'" in out
    assert "slugMatchup: Python='BRA @ ALP' R='BRA x ALP'" in out


def test_missing_output_file(copy_case, capsys):
    case = copy_case("fixture")
    (case / "outputs" / "scored_picks.csv").unlink()
    code, out = run(case, capsys)
    assert code == 1
    assert "scored_picks: FAIL\n    R output file is missing" in out
    assert "validate: points_match_status: FAIL\n    cannot check: outputs/scored_picks.csv is missing" in out


# --- pool: data checks -----------------------------------------------------


def test_wins_plus_losses_must_equal_games(copy_case, capsys):
    case = copy_case("fixture")
    replace_once(
        case / "outputs" / "game_results.csv",
        '"Bravo","BRA @ ALP","BRA","ALP","BRA",0,1,0,1,0',
        '"Bravo","BRA @ ALP","BRA","ALP","BRA",0,1,1,1,0',
    )
    code, out = run(case, capsys)
    assert code == 1
    assert "validate: games_add_up: FAIL" in out
    assert "'Team Name': 'Bravo', 'games': 1, 'wins_plus_losses': 2.0" in out


def test_grid_needs_every_day(copy_case, capsys):
    case = copy_case("fixture")
    replace_once(case / "outputs" / "standings_grid.csv", '"Bravo",2026-10-21,0,1,"0-1",0,50,"UNDER"\n', "")
    code, out = run(case, capsys)
    assert code == 1
    assert "validate: grid_complete: FAIL" in out
    assert "teams without exactly one row per day 2026-10-20 to 2026-10-21" in out


def test_rows_today_must_be_yesterday(copy_case, capsys):
    case = copy_case("fixture")
    replace_once(
        case / "outputs" / "rows_today.csv",
        '2026-10-21,"P1","Alpha","1-1",50,50,-15',
        '2026-10-20,"P1","Alpha","1-1",50,50,-15',
    )
    code, out = run(case, capsys)
    assert code == 1
    assert "validate: rows_today_date: FAIL" in out


def test_scored_pick_without_a_pick(copy_case, capsys):
    case = copy_case("fixture")
    replace_once(case / "inputs" / "picks.csv", '"Bravo","P1",14,"UNDER"\n', "")
    code, out = run(case, capsys)
    assert code == 1
    assert "validate: points_match_status: FAIL" in out
    assert "'wage': None" in out


# --- seeding ---------------------------------------------------------------


def _swap_lines(path: Path, i: int, j: int) -> None:
    lines = path.read_text().splitlines(keepends=True)
    lines[i], lines[j] = lines[j], lines[i]
    path.write_text("".join(lines))


def test_broken_seed_order(copy_case, capsys):
    case = copy_case("seeding_1")
    _swap_lines(case / "outputs" / "seeding.csv", 1, 9)  # East seeds 1 and 9 swap rows
    code, out = run(case, capsys)
    assert code == 1
    assert "  seeding: FAIL" in out
    assert "validate: seeds_contiguous: FAIL" in out
    assert "validate: pct_non_increasing: FAIL" in out
    assert "validate: seed_note_matches: ok" in out


def test_wrong_seed_note_and_duplicate_team(copy_case, capsys):
    case = copy_case("seeding_2")
    path = case / "outputs" / "seeding.csv"
    lines = path.read_text().splitlines(keepends=True)
    lines[1] = lines[1].replace('"Playoff"', '"Play-In"')
    lines.append(lines[-1])
    path.write_text("".join(lines))
    code, out = run(case, capsys)
    assert code == 1
    assert "validate: seed_note_matches: FAIL" in out
    assert "validate: teams_unique: FAIL" in out
    assert "row count differs: Python 30, R 31" in out


def test_missing_seeding_output(copy_case, capsys):
    case = copy_case("seeding_3")
    (case / "outputs" / "seeding.csv").unlink()
    code, out = run(case, capsys)
    assert code == 1
    assert out.count("cannot check: outputs/seeding.csv is missing") == 4


# --- CLI edge cases ----------------------------------------------------------


def test_folder_that_is_not_a_case(tmp_path, capsys):
    code, out = run(tmp_path, capsys)
    assert code == 1
    assert "ERROR" in out and "neither a pool nor a seeding case" in out


def test_no_arguments_prints_usage(capsys, monkeypatch):
    monkeypatch.setattr("sys.argv", ["ouanalytics-check"])
    assert main() == 2
    assert "usage: ouanalytics-check" in capsys.readouterr().err


def test_compare_table_unordered_ignores_row_order():
    a = pl.DataFrame({"x": [1.0, 2.0], "y": ["a", "b"]})
    r = pl.DataFrame({"x": ["2", "1"], "y": ["b", "a"]})
    assert compare_table("t", a, r, ordered=False).ok
    res = compare_table("t", a, r, ordered=True)
    assert not res.ok and "2 of 2 rows differ" in res.details[0]


def test_result_lines():
    assert Result("t", True).lines("  ") == ["  t: ok"]
    assert Result("t", False, ["why"]).lines() == ["t: FAIL", "    why"]


@pytest.mark.parametrize("n_bad", [6])
def test_at_most_five_rows_are_shown(n_bad):
    a = pl.DataFrame({"x": [float(i) for i in range(n_bad)]})
    r = pl.DataFrame({"x": [str(i + 0.5) for i in range(n_bad)]})
    res = compare_table("t", a, r, ordered=True)
    assert res.details[0] == f"{n_bad} of {n_bad} rows differ (first 5 shown):"
    assert len(res.details) == 6
