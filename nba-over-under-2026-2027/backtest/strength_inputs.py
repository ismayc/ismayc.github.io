"""Recompute strength_inputs.R's team-level scores in Python and compare.

Same data, same model: the prior (20 games) sits between the pool's line
(center 0) and DARKO (center 1); the record is wins and losses (pyth 0) or
the record the points imply (pyth 1); the team rate is a beta with the
evidence at half weight; Over means final wins above the line.

Run from this folder, after strength_inputs.R:
  uvx --with pandas --with python-calamine --with scipy python strength_inputs.py
Exits 1 if any score differs from strength_inputs_team.csv by more than 1e-9.
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.special import betaln, gammaln

REPO = Path(__file__).resolve().parents[2]
PRIOR_GAMES, STRENGTH_WEIGHT, PYTH_EXP = 20, 0.5, 14
FOLDERS = {"2021-22": "2021-2022", "2022-23": "2022-2023", "2023-24": "2023-2024",
           "2024-25": "2024-2025", "2025-26": "2025-2026"}

lines = pd.concat(
    pd.read_excel(REPO / f"nba-over-under-{f}" / "picks.xlsx", sheet_name="projections",
                  engine="calamine")
    .assign(season=s)[["season", "team", "win_projection"]]
    for s, f in FOLDERS.items()).rename(columns={"win_projection": "line"})
darko = pd.read_csv(REPO / "nba-over-under-META" / "preseason_projections.csv")
darko = darko[darko.source == "darko"][["season", "team", "proj_wins"]].rename(
    columns={"proj_wins": "darko"})

g = pd.read_csv("game_scores_2021_26.csv").sort_values(["season", "team", "date"], kind="stable")
grp = g.groupby(["season", "team"])
g["gp"] = grp.cumcount() + 1
g["w"] = grp["win"].cumsum()
g["pf"] = grp["pts"].cumsum().astype(float)
g["pa"] = grp["opp_pts"].cumsum().astype(float)
g["final"] = grp["win"].transform("sum")
chk = g[g.gp.isin(range(5, 80, 5))].merge(lines, on=["season", "team"]).merge(
    darko, on=["season", "team"])
assert len(chk) == 150 * 15
chk["rem"] = 82 - chk.gp
chk["rem_w"] = chk.final - chk.w
chk["over"] = chk.final > chk.line
chk["w_pyth"] = chk.gp * chk.pf**PYTH_EXP / (chk.pf**PYTH_EXP + chk.pa**PYTH_EXP)


def lchoose(n, x):
    return gammaln(n + 1) - gammaln(x + 1) - gammaln(n - x + 1)


def p_over_bb(w, rem, line, a, b):
    out = np.empty(len(w))
    need = np.floor(line) + 1 - w
    for i in range(len(w)):
        n = rem[i]
        if need[i] <= 0:
            out[i] = 1.0
        elif need[i] > n:
            out[i] = 0.0
        else:
            x = np.arange(need[i], n + 1)
            out[i] = np.exp(lchoose(n, x) + betaln(x + a[i], n - x + b[i]) - betaln(a[i], b[i])).sum()
    return out


def score(d):
    p = np.clip(np.where(d.over, d.p_over, 1 - d.p_over), 1e-6, 1 - 1e-6)
    return pd.Series({"n": len(d), "mae_rest": (d.rem * d.rate - d.rem_w).abs().mean(),
                      "brier": ((d.p_over - d.over) ** 2).mean(), "logloss": -np.log(p).mean(),
                      "right_side": ((d.p_over > 0.5) == d.over).mean()})


rows = []
for center in (0, 0.5, 1):
    for pyth in (0, 0.5, 1):
        q = (chk.line + center * (chk.darko - chk.line)) / 82
        w_used = chk.w + pyth * (chk.w_pyth - chk.w)
        a = (STRENGTH_WEIGHT * (w_used + PRIOR_GAMES * q)).to_numpy()
        b = (STRENGTH_WEIGHT * (chk.gp - w_used + PRIOR_GAMES * (1 - q))).to_numpy()
        d = chk.assign(rate=a / (a + b), p_over=p_over_bb(
            chk.w.to_numpy(), chk.rem.to_numpy(), chk.line.to_numpy(), a, b))
        d["stage"] = pd.cut(d.gp, [0, 20, 45, 75], labels=["games 5-20", "25-45", "50-75"]).astype(str)
        rows.append(score(d).to_dict() | {"center": center, "pyth": pyth, "stage": "all"})
        for st, dd in d.groupby("stage"):
            rows.append(score(dd).to_dict() | {"center": center, "pyth": pyth, "stage": st})
py = pd.DataFrame(rows)

r = pd.read_csv("strength_inputs_team.csv")
both = r.merge(py, on=["center", "pyth", "stage"], suffixes=("_r", "_py"))
assert len(both) == len(r) == len(py) == 36
worst = max((both[f"{m}_r"] - both[f"{m}_py"]).abs().max()
            for m in ("n", "mae_rest", "brier", "logloss", "right_side"))
print(f"{len(both)} rows compared with R; largest difference in any score: {worst:.2e}")
print(py[py.stage == "all"][["center", "pyth", "mae_rest", "brier", "logloss", "right_side"]]
      .round(4).to_string(index=False))
sys.exit(0 if worst < 1e-9 else 1)
