"""NBA playoff seeding with the tie-break procedure of ouanalytics/R/seeding.R.

Control flow is plain Python; polars does the filtering and counting. Where
R has separate branches that give the same result (a one-team "tie", or a
two-team group handed to the multi-team procedure, which calls the two-team
one with the same arguments) this module uses a single path.

Final fallback: alphabetical order of abbreviations, as R's ``sort()`` gives
for upper-case three-letter codes.
"""

from __future__ import annotations

import math
from collections.abc import Sequence

import polars as pl

Order = list[str]


def record_vs(team_abbr: str, opponent_abbrs: Sequence[str], scores_tidy: pl.DataFrame) -> dict:
    """W-L record of one team against a set of opponents. pct is None with no games."""
    counts = scores_tidy.filter(
        (pl.col("team_slug") == team_abbr)
        & pl.col("opponent_slug").is_in(pl.Series(list(opponent_abbrs), dtype=pl.String).implode())
    ).select(
        wins=(pl.col("score") > pl.col("opponent_score")).sum(),
        losses=(pl.col("score") < pl.col("opponent_score")).sum(),
    )
    wins, losses = counts.row(0)
    total = wins + losses
    return {
        "abbreviation": team_abbr,
        "wins": wins,
        "losses": losses,
        "pct": wins / total if total > 0 else None,
    }


def point_differential(team_abbr: str, scores_tidy: pl.DataFrame) -> float:
    """Net points over all of a team's games (missing scores skipped)."""
    return (
        scores_tidy.filter(pl.col("team_slug") == team_abbr)
        .select((pl.col("score") - pl.col("opponent_score")).sum())
        .item()
    )


def _meta_value(meta: pl.DataFrame, abbr: str, field: str) -> str:
    return meta.filter(pl.col("abbreviation") == abbr)[field].item(0)


def _members(meta: pl.DataFrame, field: str, value: str) -> list[str]:
    return meta.filter(pl.col(field) == value)["abbreviation"].to_list()


def _by(t1: str, t2: str, v1, v2) -> Order | None:
    """Higher value first; None when either is missing or they are level."""
    if v1 is None or v2 is None or v1 == v2:
        return None
    return [t1, t2] if v1 > v2 else [t2, t1]


def get_division_leaders(
    conf_standings: pl.DataFrame, scores_tidy: pl.DataFrame, meta: pl.DataFrame
) -> list[str]:
    """The best team in each division by Winning Pct; co-leaders are separated
    with the tie-break procedure, with no division leaders known yet."""
    leaders: list[str] = []
    for div in conf_standings["division"].unique(maintain_order=True).to_list():
        div_teams = conf_standings.filter(pl.col("division") == div)
        top = div_teams.filter(pl.col("Winning Pct") == pl.col("Winning Pct").max())
        ordered = break_multi_team_tie(
            top["abbreviation"].to_list(), conf_standings, scores_tidy, meta
        )
        leaders.extend(ordered[:1])
    return leaders


def break_two_team_tie(
    t1: str,
    t2: str,
    standings_df: pl.DataFrame,
    scores_tidy: pl.DataFrame,
    meta: pl.DataFrame,
    division_leaders: Sequence[str] = (),
    playoff_eligible_own: Sequence[str] | None = None,
    playoff_eligible_other: Sequence[str] | None = None,
) -> Order:
    """Order two tied teams: [winner, loser]."""
    # (1) head-to-head
    h2h = record_vs(t1, [t2], scores_tidy)
    if found := _by(t1, t2, h2h["wins"], h2h["losses"]):
        return found
    # (2) a division leader beats a non-leader
    if found := _by(t1, t2, t1 in division_leaders, t2 in division_leaders):
        return found
    # (3) division record, only when both are in the same division
    division = _meta_value(meta, t1, "division")
    opponent_sets = []
    if division == _meta_value(meta, t2, "division"):
        opponent_sets.append(_members(meta, "division", division))
    # (4) conference record
    opponent_sets.append(_members(meta, "conference", _meta_value(meta, t1, "conference")))
    # (5), (6) record vs playoff-eligible teams in own, then other conference
    opponent_sets += [s for s in (playoff_eligible_own, playoff_eligible_other) if s]
    for opps in opponent_sets:
        r1 = record_vs(t1, opps, scores_tidy)["pct"]
        r2 = record_vs(t2, opps, scores_tidy)["pct"]
        if found := _by(t1, t2, r1, r2):
            return found
    # (7) point differential
    if found := _by(t1, t2, point_differential(t1, scores_tidy), point_differential(t2, scores_tidy)):
        return found
    return sorted([t1, t2])


def break_multi_team_tie(
    team_abbrs: Sequence[str],
    standings_df: pl.DataFrame,
    scores_tidy: pl.DataFrame,
    meta: pl.DataFrame,
    division_leaders: Sequence[str] = (),
    playoff_eligible_own: Sequence[str] | None = None,
    playoff_eligible_other: Sequence[str] | None = None,
) -> Order:
    """Order any number of tied teams, best first."""
    remaining = list(team_abbrs)
    if len(remaining) <= 1:
        return remaining
    if len(remaining) == 2:
        return break_two_team_tie(
            remaining[0], remaining[1], standings_df, scores_tidy, meta,
            division_leaders, playoff_eligible_own, playoff_eligible_other,
        )

    def recurse(teams: list[str]) -> Order:
        return break_multi_team_tie(
            teams, standings_df, scores_tidy, meta,
            division_leaders, playoff_eligible_own, playoff_eligible_other,
        )

    def split(values: list) -> Order | None:
        """Best value first, both parts re-broken from the start (rule 1c).
        None when fewer than two distinct known values."""
        known = {v for v in values if v is not None}
        if len(known) < 2:
            return None
        best = [t for t, v in zip(remaining, values, strict=True) if v == max(known)]
        return recurse(best) + recurse([t for t in remaining if t not in best])

    # (1) division leaders above non-leaders
    leaders = [t for t in remaining if t in division_leaders]
    others = [t for t in remaining if t not in division_leaders]
    if leaders and others:
        return recurse(leaders) + recurse(others)

    def pcts(opps: Sequence[str]) -> list:
        return [record_vs(t, opps, scores_tidy)["pct"] for t in remaining]

    # (2) record among all tied teams
    criteria = [lambda: pcts(remaining)]
    # (3) division record, only when all are in one division
    divisions = meta.filter(pl.col("abbreviation").is_in(pl.Series(remaining, dtype=pl.String).implode()))["division"].unique().to_list()
    if len(divisions) == 1:
        criteria.append(lambda: pcts(_members(meta, "division", divisions[0])))
    # (4) conference record
    conference = _meta_value(meta, remaining[0], "conference")
    criteria.append(lambda: pcts(_members(meta, "conference", conference)))
    # (5) record vs playoff-eligible teams in own conference
    if playoff_eligible_own:
        criteria.append(lambda: pcts(playoff_eligible_own))
    # (6) point differential
    criteria.append(lambda: [point_differential(t, scores_tidy) for t in remaining])

    for values in criteria:
        if found := split(values()):
            return found
    return sorted(remaining)


def _cutoff(pcts: list[float]) -> float:
    """The 10th-best Winning Pct, or the lowest when there are fewer than 10."""
    return pcts[9] if len(pcts) >= 10 else min(pcts, default=math.inf)


def _eligible(standings: pl.DataFrame, conf: str) -> list[str]:
    """Playoff-eligible teams: Winning Pct at or above the 10th best (with ties)."""
    teams = standings.filter(pl.col("conference") == conf)
    pcts = sorted(teams["Winning Pct"].drop_nulls().to_list(), reverse=True)
    return teams.filter(pl.col("Winning Pct") >= _cutoff(pcts))["abbreviation"].to_list()


def seed_conference(
    conf: str, standings: pl.DataFrame, scores_tidy: pl.DataFrame, meta: pl.DataFrame
) -> pl.DataFrame:
    """Seed one conference: seed, team columns, and seed_note."""
    conf_standings = standings.filter(pl.col("conference") == conf).sort(
        "Winning Pct", descending=True, nulls_last=True, maintain_order=True
    )
    division_leaders = get_division_leaders(conf_standings, scores_tidy, meta)
    own = _eligible(standings, conf)
    other = _eligible(standings, "West" if conf == "East" else "East")

    groups = conf_standings.group_by("Winning Pct", maintain_order=True).agg("abbreviation")
    final_order: list[str] = []
    for teams in groups["abbreviation"].to_list():
        final_order += break_multi_team_tie(
            teams, conf_standings, scores_tidy, meta, division_leaders, own, other
        )

    seed = pl.col("seed")
    return (
        pl.DataFrame({"seed": range(1, len(final_order) + 1), "abbreviation": final_order})
        .join(conf_standings, on="abbreviation", how="inner", maintain_order="left")
        .with_columns(
            seed_note=pl.when(seed <= 6)
            .then(pl.lit("Playoff"))
            .when(seed <= 10)
            .then(pl.lit("Play-In"))
            .otherwise(pl.lit(""))
        )
        .select(
            "seed", "team_name", "conference", "division", "wins", "losses",
            "Winning Pct", "Games Back", "abbreviation", "seed_note",
        )
    )


def compute_playoff_seeding(
    standings: pl.DataFrame, scores_tidy: pl.DataFrame, meta: pl.DataFrame
) -> pl.DataFrame:
    """Seed both conferences: East then West."""
    return pl.concat(
        [seed_conference(c, standings, scores_tidy, meta) for c in ("East", "West")]
    )
