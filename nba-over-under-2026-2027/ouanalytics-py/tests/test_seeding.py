"""Direct checks of the tie-break helpers on tiny made-up leagues. The six
golden seeding cases already reach every branch against R; these pin the
rules in a readable form."""

from __future__ import annotations

import polars as pl

from ouanalytics_py.seeding import (
    break_multi_team_tie,
    break_two_team_tie,
    point_differential,
    record_vs,
    seed_conference,
)

META = pl.DataFrame(
    {
        "team": ["a", "b", "c", "d"],
        "conference": ["East"] * 4,
        "division": ["Atl", "Atl", "Cen", "Cen"],
        "abbreviation": ["AAA", "BBB", "CCC", "DDD"],
    }
)


def scores(*games):
    """games as (winner, loser, winner_pts, loser_pts); one row per team."""
    rows = []
    for w, lo, wp, lp in games:
        rows += [(w, lo, float(wp), float(lp)), (lo, w, float(lp), float(wp))]
    return pl.DataFrame(rows, schema=["team_slug", "opponent_slug", "score", "opponent_score"], orient="row")


def test_record_vs_without_games_has_no_pct():
    r = record_vs("AAA", ["BBB"], scores(("CCC", "DDD", 100, 90)))
    assert r == {"abbreviation": "AAA", "wins": 0, "losses": 0, "pct": None}


def test_point_differential_sums_all_games():
    st = scores(("AAA", "BBB", 100, 90), ("CCC", "AAA", 101, 100))
    assert point_differential("AAA", st) == 9
    assert point_differential("DDD", st) == 0


def test_two_team_head_to_head_then_alphabetical():
    st = scores(("BBB", "AAA", 100, 99))
    assert break_two_team_tie("AAA", "BBB", None, st, META) == ["BBB", "AAA"]
    # No games at all: every criterion is level, so alphabetical.
    empty = scores()
    assert break_two_team_tie("BBB", "AAA", None, empty, META) == ["AAA", "BBB"]


def test_two_team_division_leader_first():
    assert break_two_team_tie("AAA", "CCC", None, scores(), META, ["CCC"]) == ["CCC", "AAA"]


def test_multi_team_record_among_tied_then_restart():
    # AAA beats both; BBB and CCC split nothing, so BBB vs CCC restarts with
    # the two-team rules: BBB has the better point differential.
    st = scores(("AAA", "BBB", 100, 99), ("AAA", "CCC", 100, 90), ("BBB", "DDD", 120, 90))
    assert break_multi_team_tie(["CCC", "BBB", "AAA"], None, st, META) == ["AAA", "BBB", "CCC"]


def test_seed_conference_notes_and_short_conference():
    standings = pl.DataFrame(
        {
            "team_name": ["a", "b", "c", "d"],
            "conference": ["East"] * 4,
            "division": ["Atl", "Atl", "Cen", "Cen"],
            "wins": [3.0, 2, 1, 0],
            "losses": [0.0, 1, 2, 3],
            "Winning Pct": [1.0, 2 / 3, 1 / 3, 0.0],
            "Games Back": ["-", "1.0", "2.0", "3.0"],
            "abbreviation": ["AAA", "BBB", "CCC", "DDD"],
        }
    )
    out = seed_conference("East", standings, scores(), META)
    assert out["abbreviation"].to_list() == ["AAA", "BBB", "CCC", "DDD"]
    assert out["seed_note"].to_list() == ["Playoff"] * 4
    # No West teams at all: the other conference has no eligible teams.
    assert out["seed"].to_list() == [1, 2, 3, 4]
