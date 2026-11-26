#!/usr/bin/env python3
"""Daily NBA summary email: the over/under pool and the fantasy draft.

Writes email.html, subject.txt, and summary.json into RUN_DIR. send.sh on the
Mac mini runs it at 4:30 AM Phoenix and sends the result with Gmail.

Inputs (nothing is recomputed that a page already computes):
  * Over/under pool: the tables the 2:00 AM mini build exports to
    check-export/pool (ouanalytics compute_pool), the chance-to-win cache
    (chance.csv, dumped from rds/chance_cache.rds by send.sh), and
    current_year.csv (team box scores).
  * Fantasy draft: the public JSON behind nba-fantasy-draft.netlify.app
    (players.json for the picks, /api/stats/<season>/history for the saved
    daily points). Roster strength follows the player tracker's code
    (web/players_template.html: statFields, scoreRows, rosterTotals,
    pointRows); check_strength.mjs runs that page code and compares.

Python 3.9 standard library only (the mini's /usr/bin/python3).

Usage:
  daily_email.py RUN_DIR [--date YYYY-MM-DD] [--ou-dir DIR] [--chance CSV]
                 [--box CSV] [--fantasy-dir DIR]
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import html
import json
import math
import os
import sys
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
PROJECT = os.path.dirname(os.path.dirname(HERE))   # nba-over-under-2026-2027

OU_URL = "https://ismayc.github.io/2027-nba-over-under.html"
FANTASY_SEASON = "2026-27"
FANTASY_BASE = "https://nba-fantasy-draft.netlify.app"
FANTASY_URL = f"{FANTASY_BASE}/{FANTASY_SEASON}/"
FANTASY_PLAYERS_URL = f"{FANTASY_BASE}/{FANTASY_SEASON}/players/"

# Same order and keys as STAT_KEYS in netlify/lib/history.mts
STAT_KEYS = ["gp", "per", "pts", "reb", "ast", "stl", "blk", "tov", "fgm",
             "fga", "ftm", "fta", "oreb", "dreb", "pf"]
COUNT_KEYS = ["pts", "reb", "ast", "stl", "blk", "tov", "fgm", "fga", "ftm",
              "fta", "oreb", "dreb", "pf"]

OVER_COL, UNDER_COL = "#2457a6", "#b8322f"
UP_COL, DOWN_COL = "#1f7a3a", "#b8322f"
INK, DIM, RULE, PAPER, CARD = "#1d2a22", "#5d6b62", "#d9d2bf", "#f4efe2", "#fffdf7"
HEAD = "#1f4d3a"


# ---------------------------------------------------------------- helpers
def read_csv(path):
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def num(x):
    if x is None or x == "" or x == "NA":
        return None
    return float(x)


def js_round(x):
    """Math.round: halves go up (Python's round() goes to even)."""
    return math.floor(x + 0.5)


def iso(d):
    return d.isoformat()


def long_day(d):
    return f"{d.strftime('%B')} {d.day}"


def short_day(d):
    return f"{d.strftime('%b')} {d.day}"


def e(s):
    return html.escape(str(s), quote=True)


def fetch_json(url, cache_dir=None, name=None):
    if cache_dir:
        with open(os.path.join(cache_dir, name), encoding="utf-8") as f:
            return json.load(f)
    req = urllib.request.Request(url, headers={"User-Agent": "nba-daily-email/1.0"})
    with urllib.request.urlopen(req, timeout=60) as r:
        return json.loads(r.read().decode("utf-8"))


def signed(v, digits=0, zero="0"):
    if v is None:
        return ""
    r = round(v, digits)
    if r == 0:
        return zero
    s = f"{abs(r):,.{digits}f}"
    return ("+" if r > 0 else "−") + s


def delta_cell(v, digits=0, unit="", good_up=True):
    """A colored change, or a dim dash for none."""
    if v is None:
        return f'<span style="color:{DIM}">n/a</span>'
    r = round(v, digits)
    if r == 0:
        return f'<span style="color:{DIM}">0{unit}</span>'
    col = UP_COL if (r > 0) == good_up else DOWN_COL
    return f'<span style="color:{col};font-weight:600">{signed(v, digits)}{unit}</span>'


def pct(v, digits=0):
    return "" if v is None else f"{100 * v:.{digits}f}%"


# ---------------------------------------------------------------- over/under
def rank_entries(rows):
    """Place by points, then number right, then 15s, 14s, 13s, and 12s right
    (the page's tie-breakers). Ties on every key share the place."""
    def key(r):
        return tuple(-num(r[k]) for k in (
            "Projected Total Points", "Number Correct", "Number (Wage 15) Correct",
            "Number (Wage 14) Correct", "Number (Wage 13) Correct",
            "Number (Wage 12) Correct"))
    ordered = sorted(rows, key=key)
    out, last_key, last_place = {}, None, 0
    for i, r in enumerate(ordered, 1):
        k = key(r)
        place = last_place if k == last_key else i
        out[r["Player"]] = place
        last_key, last_place = k, place
    return out


def latest_on_or_before(dates, target):
    c = [d for d in dates if d <= target]
    return max(c) if c else None


def over_under(ou_dir, chance_csv, box_csv, day):
    """Everything the email says about the pool, for games through `day`."""
    out_dir = os.path.join(ou_dir, "outputs")
    params = {r["name"]: r["value"] for r in read_csv(os.path.join(ou_dir, "inputs", "params.csv"))}
    built_through = dt.date.fromisoformat(params["yesterday"])

    score = read_csv(os.path.join(out_dir, "projected_score.csv"))
    by_date = {}
    for r in score:
        by_date.setdefault(dt.date.fromisoformat(r["Date"]), []).append(r)
    dates = sorted(by_date)
    d1 = latest_on_or_before(dates, day)
    d0 = latest_on_or_before(dates, day - dt.timedelta(days=1))
    dw = latest_on_or_before(dates, day - dt.timedelta(days=7))

    def pts(d):
        return {r["Player"]: num(r["Projected Total Points"]) for r in by_date.get(d, [])}

    def correct(d):
        return {r["Player"]: int(num(r["Number Correct"])) for r in by_date.get(d, [])}

    p1, p0, pw = pts(d1), pts(d0), pts(dw)
    r1, r0, rw = rank_entries(by_date[d1]), rank_entries(by_date.get(d0, [])), rank_entries(by_date.get(dw, []))
    c1 = correct(d1)

    chance = {}
    for r in read_csv(chance_csv):
        chance[(dt.date.fromisoformat(r["date"]), r["player"])] = (num(r["p_first"]), num(r["p_top4"]), num(r["exp_place"]))
    cdates = sorted({k[0] for k in chance})
    cd1 = latest_on_or_before(cdates, day)
    cd0 = latest_on_or_before(cdates, day - dt.timedelta(days=1))
    cdw = latest_on_or_before(cdates, day - dt.timedelta(days=7))

    def ch(d, p, i):
        v = chance.get((d, p))
        return v[i] if v else None

    entries = []
    for p in p1:
        entries.append({
            "entry": p, "place": r1.get(p), "points": p1[p], "correct": c1.get(p),
            "d_day": None if d0 is None else p1[p] - p0.get(p, 0),
            "d_week": None if dw is None else p1[p] - pw.get(p, 0),
            "place_day": r0.get(p), "place_week": rw.get(p),
            "p_first": ch(cd1, p, 0), "p_top4": ch(cd1, p, 1),
            "p_first_day": None if cd0 is None or ch(cd0, p, 0) is None else ch(cd1, p, 0) - ch(cd0, p, 0),
            "p_first_week": None if cdw is None or ch(cdw, p, 0) is None else ch(cd1, p, 0) - ch(cdw, p, 0),
        })
    entries.sort(key=lambda x: (x["place"], x["entry"]))

    # Team status and each entry's points from each team, by date
    teams = {}
    for r in read_csv(os.path.join(out_dir, "player_projections_by_team.csv")):
        d = dt.date.fromisoformat(r["Date"])
        if d not in (d1, d0, dw):
            continue
        t = teams.setdefault(r["Team"], {}).setdefault(d, {"entries": {}})
        t["status"] = r["Current Over/Under"] or None
        t["record"] = r["Current Record"]
        t["pace"] = num(r["Current Win %"])
        t["line_pct"] = num(r["Vegas Win Projection %"])
        t["determined"] = r["Outcome Determined"]
        t["wins_to_over"] = num(r["Wins To Go Over Vegas"])
        t["entries"][r["Player"]] = (r["Over/Under Pick"], num(r["Current Projected Points"]) or 0)

    lines = {r["team"]: num(r["win_projection"]) for r in read_csv(os.path.join(ou_dir, "inputs", "projections.csv"))}

    def flips(a, b):
        out = []
        if a is None or b is None:
            return out
        for team, by in teams.items():
            if a not in by or b not in by:
                continue
            s0, s1 = by[a]["status"], by[b]["status"]
            if s0 == s1 or s1 is None:
                continue
            gain = sorted(p for p, (pick, _) in by[b]["entries"].items() if pick == s1)
            lose = sorted(p for p, (pick, _) in by[b]["entries"].items() if pick != s1)
            swing = {p: by[b]["entries"][p][1] - by[a]["entries"].get(p, ("", 0))[1] for p in by[b]["entries"]}
            out.append({"team": team, "from": s0, "to": s1, "record": by[b]["record"],
                        "line": lines.get(team), "pace": by[b]["pace"],
                        "gain": gain, "lose": lose, "swing": swing})
        out.sort(key=lambda x: -max(abs(v) for v in x["swing"].values()) if x["swing"] else 0)
        return out

    clinched = []
    if d0 is not None:
        for team, by in teams.items():
            if d1 in by and d0 in by and by[d1]["determined"] != "not yet" and by[d0]["determined"] == "not yet":
                clinched.append({"team": team, "outcome": by[d1]["determined"]})

    # Team games from the box scores (two rows a game)
    box = read_csv(box_csv)
    games = {}
    for r in box:
        games.setdefault(r["GAME_ID"], []).append(r)

    def team_games(lo, hi):
        rows = []
        for gid, pair in games.items():
            if len(pair) != 2:
                continue
            gd = dt.date.fromisoformat(pair[0]["GAME_DATE"])
            if lo <= gd <= hi:
                rows.append((gd, pair))
        return rows

    last_night = []
    for gd, pair in team_games(day, day):
        a, b = pair
        home = b if " vs. " in b["MATCHUP"] else a
        away = a if home is b else b
        hp, ap = int(num(home["PTS"])), int(num(away["PTS"]))
        win = home if hp > ap else away
        lose = away if win is home else home
        last_night.append({
            "away": away["TEAM_NAME"], "home": home["TEAM_NAME"],
            "away_abbr": away["TEAM_ABBREVIATION"], "home_abbr": home["TEAM_ABBREVIATION"],
            "away_pts": ap, "home_pts": hp, "margin": abs(hp - ap),
            "winner": win["TEAM_NAME"], "loser": lose["TEAM_NAME"],
            "ot": int(num(a["MIN"])) > 240 + 5,
            "fg3m": {away["TEAM_NAME"]: int(num(away["FG3M"])), home["TEAM_NAME"]: int(num(home["FG3M"]))},
        })
    last_night.sort(key=lambda g: -g["margin"])

    week_lo = day - dt.timedelta(days=6)
    rec = {}
    for gd, pair in team_games(week_lo, day):
        for r in pair:
            t = rec.setdefault(r["TEAM_NAME"], {"w": 0, "l": 0, "pm": 0.0, "pts": 0, "g": 0})
            t["w" if r["WL"] == "W" else "l"] += 1
            t["pm"] += num(r["PLUS_MINUS"])
            t["pts"] += int(num(r["PTS"]))
            t["g"] += 1
    week_teams = sorted(
        ({"team": k, **v, "pm_g": v["pm"] / v["g"]} for k, v in rec.items()),
        key=lambda x: (-(x["w"] - x["l"]), -x["pm_g"]))

    return {
        "built_through": iso(built_through), "stale": built_through < day,
        "day": iso(d1) if d1 else None, "prev": iso(d0) if d0 else None,
        "week": iso(dw) if dw else None, "chance_day": iso(cd1) if cd1 else None,
        "entries": entries, "flips_day": flips(d0, d1), "flips_week": flips(dw, d1),
        "clinched": clinched, "last_night": last_night,
        "week_from": iso(week_lo), "week_teams": week_teams,
    }


# ---------------------------------------------------------------- fantasy
def fp_of(s):
    return s["pts"] + 1.2 * s["reb"] + 1.5 * s["ast"] + 3 * s["stl"] + 3 * s["blk"] - s["tov"]


def gmsc_of(s):
    if s.get("fga") is None:
        return None
    return (s["pts"] + 0.4 * s["fgm"] - 0.7 * s["fga"] - 0.4 * (s["fta"] - s["ftm"])
            + 0.7 * s["oreb"] + 0.3 * s["dreb"] + s["stl"] + 0.7 * s["ast"]
            + 0.7 * s["blk"] - 0.4 * s["pf"] - s["tov"])


def percentiles(rows, key):
    vals = sorted(r[key] for r in rows if r[key] is not None)

    def f(v):
        if v is None or not vals:
            return None
        below = sum(1 for x in vals if x < v)
        equal = sum(1 for x in vals if x == v)
        return 100.0 if len(vals) == 1 else 100 * (below + (equal - 1) / 2) / (len(vals) - 1)
    return f


def point_rows(point, ballot, manager_of):
    """pointRows + statFields + scoreRows from the player tracker."""
    by_name = {a[0]: a for a in point["players"]}
    most_gp = max([w + l for w, l in point["teams"].values()] or [0])
    min_gp = max(1, js_round(most_gp * 0.3))
    rows = []
    for b in ballot:
        a = by_name.get(b["name"])
        st = dict(zip(STAT_KEYS, a[2:2 + len(STAT_KEYS)])) if a and a[2] is not None else None
        team_gp = a[1] if a else 0
        row = {"name": b["name"], "manager": manager_of.get(b["name"]), "team": a[-1] if a else None,
               "s": st, "teamGP": team_gp,
               "fp": fp_of(st) if st else None, "gmsc": gmsc_of(st) if st else None,
               "per": st["per"] if st else None,
               "avail": min(1, st["gp"] / team_gp) if st and team_gp > 0 else None,
               "eligible": bool(st) and st["gp"] >= min_gp, "strength": None}
        rows.append(row)
    elig = [r for r in rows if r["eligible"]]
    p_per, p_gm, p_fp = percentiles(elig, "per"), percentiles(elig, "gmsc"), percentiles(elig, "fp")
    for r in elig:
        parts = [v for v in (p_per(r["per"]), p_gm(r["gmsc"]), p_fp(r["fp"])) if v is not None]
        r["strength"] = (sum(parts) / len(parts)) * (r["avail"] if r["avail"] is not None else 1) if parts else None
    return rows


def roster_totals(ps):
    strong = [r for r in ps if r["strength"] is not None]
    return {
        "strength": sum(r["strength"] for r in strong),
        "fp": sum(r["fp"] for r in ps if r["eligible"]),
        "best": max(strong, key=lambda r: r["strength"]) if strong else None,
    }


def totals(a):
    """Season totals from a history row: averages times games."""
    if a is None or a[2] is None:
        return None
    s = dict(zip(STAT_KEYS, a[2:2 + len(STAT_KEYS)]))
    gp = s["gp"]
    return {"gp": gp, **{k: (s[k] or 0) * gp for k in COUNT_KEYS}}


def window(new, old):
    """Box totals between two history points for each player who played."""
    old_by = {a[0]: a for a in old["players"]} if old else {}
    out = {}
    for a in new["players"]:
        t1 = totals(a)
        if t1 is None:
            continue
        t0 = totals(old_by.get(a[0])) if old else None
        g = t1["gp"] - (t0["gp"] if t0 else 0)
        if g <= 0:
            continue
        d = {k: t1[k] - (t0[k] if t0 else 0) for k in COUNT_KEYS}
        d = {k: max(0, js_round(v)) for k, v in d.items()}   # float32 averages: round to whole counts
        d["gp"] = g
        d["team"] = a[-1]
        d["fp"] = fp_of(d)
        out[a[0]] = d
    return out


def fantasy(day, cache_dir=None):
    meta = fetch_json(f"{FANTASY_BASE}/{FANTASY_SEASON}/players.json", cache_dir, "players.json")
    hist = fetch_json(f"{FANTASY_BASE}/api/stats/{FANTASY_SEASON}/history", cache_dir, "history.json")
    ballot = meta["ballot"]
    manager_of = {r["name"]: r["player"] for r in meta["results"]}
    managers = list(meta["draft_order"])
    points = sorted(hist.get("points", []), key=lambda p: p["date"])
    by_date = {dt.date.fromisoformat(p["date"]): p for p in points}
    dates = sorted(by_date)
    d1 = latest_on_or_before(dates, day)
    if d1 is None:
        return None
    d0 = latest_on_or_before(dates, d1 - dt.timedelta(days=1))
    dw = latest_on_or_before(dates, day - dt.timedelta(days=7))

    def standings(d):
        if d is None:
            return None
        rows = point_rows(by_date[d], ballot, manager_of)
        return {m: roster_totals([r for r in rows if r["manager"] == m]) for m in managers}, rows

    s1, rows1 = standings(d1)
    s0 = standings(d0)[0] if d0 else None
    sw = standings(dw)[0] if dw else None

    def places(s):
        order = sorted(s, key=lambda m: -s[m]["strength"])
        return {m: i + 1 for i, m in enumerate(order)}

    pl1 = places(s1)
    pl0 = places(s0) if s0 else {}
    plw = places(sw) if sw else {}
    table = []
    for m in sorted(managers, key=lambda m: pl1[m]):
        best = s1[m]["best"]
        table.append({
            "manager": m, "place": pl1[m], "place_day": pl0.get(m), "place_week": plw.get(m),
            "strength": s1[m]["strength"], "fp": s1[m]["fp"],
            "d_day": s1[m]["strength"] - s0[m]["strength"] if s0 else None,
            "d_week": s1[m]["strength"] - sw[m]["strength"] if sw else None,
            "fp_week": s1[m]["fp"] - sw[m]["fp"] if sw else None,
            "best": None if best is None else {"name": best["name"].split(",")[0], "strength": best["strength"]},
        })

    games_day = d1 == day
    night = window(by_date[d1], by_date[d0]) if (games_day and d0) else {}
    week = window(by_date[d1], by_date[dw]) if dw else {}

    def top(w, n, key, min_gp=1):
        rows = [dict(name=k, manager=manager_of.get(k), **v) for k, v in w.items() if v["gp"] >= min_gp]
        return sorted(rows, key=key)[:n]

    night_top = top(night, 10, lambda r: -r["fp"])
    week_top = top(week, 10, lambda r: (-(r["fp"] / r["gp"]), -r["gp"]), min_gp=2)
    week_by_mgr = {m: 0.0 for m in managers}
    games_by_mgr = {m: 0 for m in managers}
    for k, v in week.items():
        m = manager_of.get(k)
        if m:
            week_by_mgr[m] += v["fp"]
            games_by_mgr[m] += v["gp"]
    mgr_week = sorted(({"manager": m, "fp": week_by_mgr[m], "games": games_by_mgr[m]} for m in managers),
                      key=lambda x: -x["fp"])
    undrafted_night = [r for r in night_top if not r["manager"]]
    return {
        "day": iso(d1), "prev": iso(d0) if d0 else None, "week": iso(dw) if dw else None,
        "games_day": games_day, "asOf": by_date[d1]["asOf"],
        "table": table, "night_top": night_top, "week_top": week_top, "mgr_week": mgr_week,
        "undrafted_night": undrafted_night,
    }


# ---------------------------------------------------------------- HTML
TH = f'style="text-align:right;padding:5px 5px;border-bottom:2px solid {INK};font-size:12px;color:{DIM};font-weight:600;white-space:nowrap"'
THL = TH.replace("text-align:right", "text-align:left")
TD = f'style="text-align:right;padding:5px 5px;border-bottom:1px solid {RULE};font-size:13px;white-space:nowrap"'
TDL = TD.replace("text-align:right", "text-align:left").replace("white-space:nowrap", "white-space:normal")


def table(head, rows, left_cols=(0,)):
    h = "".join(f"<th {THL if i in left_cols else TH}>{c}</th>" for i, c in enumerate(head))
    b = "".join("<tr>" + "".join(f"<td {TDL if i in left_cols else TD}>{c}</td>" for i, c in enumerate(r)) + "</tr>" for r in rows)
    return (f'<table role="presentation" cellpadding="0" cellspacing="0" '
            f'style="border-collapse:collapse;width:100%;margin:6px 0 4px">'
            f"<thead><tr>{h}</tr></thead><tbody>{b}</tbody></table>")


def section(title, inner, sub=""):
    s = f'<p style="margin:0 0 6px;color:{DIM};font-size:13px">{sub}</p>' if sub else ""
    return (f'<div style="background:{CARD};border:1px solid {RULE};border-radius:8px;'
            f'padding:16px 18px;margin:0 0 16px">'
            f'<h2 style="margin:0 0 4px;font-size:18px;color:{HEAD};font-family:Georgia,serif">{title}</h2>'
            f"{s}{inner}</div>")


def h3(t):
    return f'<h3 style="margin:14px 0 2px;font-size:15px;color:{INK}">{t}</h3>'


def note(t):
    return f'<p style="margin:6px 0 0;color:{DIM};font-size:12px;line-height:1.45">{t}</p>'


def para(t):
    return f'<p style="margin:6px 0;font-size:14px;line-height:1.5">{t}</p>'


def place_move(now, before):
    if before is None or now is None or now == before:
        return ""
    up = before - now
    col = UP_COL if up > 0 else DOWN_COL
    arrow = "▲" if up > 0 else "▼"
    return f' <span style="color:{col};font-size:11px">{arrow}{abs(up)}</span>'


def ou_word(s):
    if not s:
        return ""
    return f'<span style="color:{OVER_COL if s == "OVER" else UNDER_COL};font-weight:700">{s}</span>'


def name_list(xs):
    xs = list(xs)
    if len(xs) <= 1:
        return "".join(xs)
    if len(xs) == 2:
        return f"{xs[0]} and {xs[1]}"
    return ", ".join(xs[:-1]) + f", and {xs[-1]}"


def stat_line(r):
    bits = [f"{r['pts']} pts", f"{r['reb']} reb", f"{r['ast']} ast"]
    if r["stl"]:
        bits.append(f"{r['stl']} stl")
    if r["blk"]:
        bits.append(f"{r['blk']} blk")
    return ", ".join(bits)


def per_game(r, k):
    return f"{r[k] / r['gp']:.1f}"


def mgr_cell(m):
    return e(m) if m else f'<span style="color:{DIM}">undrafted</span>'


def render(day, ou, fa):
    D = long_day(day)
    parts = []

    # ---- headline
    heads = []
    if ou:
        lead = ou["entries"][0]
        fav = max(ou["entries"], key=lambda x: x["p_first"] or 0)
        heads.append(f"Over/under: <b>{e(lead['entry'])}</b> leads with {lead['points']:.0f} projected points. "
                     f"<b>{e(fav['entry'])}</b> has the best chance to win ({pct(fav['p_first'])}).")
        movers = [x for x in ou["entries"] if x["p_first_day"] is not None]
        if movers:
            big = max(movers, key=lambda x: abs(x["p_first_day"]))
            if abs(big["p_first_day"]) >= 0.005:
                heads.append(f"Biggest chance move since the day before: <b>{e(big['entry'])}</b> "
                             f"{signed(100 * big['p_first_day'], 1)} percentage points, to {pct(big['p_first'])}.")
        nf = len(ou["flips_day"])
        if nf:
            heads.append(f"{nf} team{'s' if nf != 1 else ''} flipped over/under status on {short_day(day)}: "
                         + name_list(f"{e(f['team'])} to {ou_word(f['to'])}" for f in ou["flips_day"]) + ".")
    if fa:
        t = fa["table"][0]
        heads.append(f"Fantasy draft: <b>{e(t['manager'])}</b> has the strongest roster ({t['strength']:.0f}).")
        if fa["night_top"]:
            b = fa["night_top"][0]
            heads.append(f"Best ballot game on {short_day(day)}: <b>{e(b['name'].split(',')[0])}</b> "
                         f"({stat_line(b)}, {b['fp']:.1f} fantasy points, {mgr_cell(b['manager'])}).")
    if ou and ou["last_night"]:
        g = ou["last_night"][0]
        heads.append(f"Biggest win on {short_day(day)}: {e(g['winner'])} by {g['margin']} "
                     f"({g['away_abbr']} {g['away_pts']}, {g['home_abbr']} {g['home_pts']}).")
    parts.append(section("Top lines", "<ul style=\"margin:4px 0 0 18px;padding:0;font-size:14px;line-height:1.55\">"
                         + "".join(f"<li style=\"margin:0 0 4px\">{h}</li>" for h in heads) + "</ul>"))

    # ---- over/under
    if ou:
        body = []
        if ou["stale"]:
            body.append(para(f'<b style="color:{DOWN_COL}">The mini build did not run today.</b> '
                             f"These pool numbers are through {long_day(dt.date.fromisoformat(ou['built_through']))}."))
        rows = []
        for x in ou["entries"]:
            rows.append([
                f"{x['place']}{place_move(x['place'], x['place_day'])}",
                f"<b>{e(x['entry'])}</b>",
                f"{x['points']:.0f}",
                delta_cell(x["d_day"]), delta_cell(x["d_week"]),
                f"{x['correct']}",
            ])
        body.append(table(["#", "Entry", "Points", "1 day", "7 days", "Right"], rows, left_cols=(0, 1)))
        body.append(h3("Chance to win"))
        rows = [[f"<b>{e(x['entry'])}</b>", pct(x["p_first"]),
                 delta_cell(None if x["p_first_day"] is None else 100 * x["p_first_day"], 1, ""),
                 delta_cell(None if x["p_first_week"] is None else 100 * x["p_first_week"], 1, ""),
                 pct(x["p_top4"])] for x in sorted(ou["entries"], key=lambda x: -(x["p_first"] or 0))]
        body.append(table(["Entry", "Win", "1 day", "7 days", "Top 4"], rows))
        body.append(note("Points: projected points if every team kept its current pace. Right: picks on the right side now. "
                         "Win and Top 4 (second table): the page's simulated chance to finish first or in the top four. "
                         "The 1-day and 7-day columns are changes; chance changes are in percentage points. "
                         "Arrows show place changes since the day before."))

        def flip_rows(fl):
            out = []
            for f in fl:
                swing = sorted(f["swing"].items(), key=lambda kv: -kv[1])
                sw = ", ".join(f"{e(p)} {signed(v)}" for p, v in swing if v)
                out.append([f"<b>{e(f['team'])}</b><br><span style=\"color:{DIM};font-size:12px\">{e(f['record'])}, line {f['line']:g}</span>",
                            f"{ou_word(f['from'])} → {ou_word(f['to'])}", sw or "no change"])
            return out
        if ou["flips_day"]:
            body.append(h3(f"Teams that flipped on {short_day(day)}"))
            body.append(table(["Team", "Status", "Points swing by entry"], flip_rows(ou["flips_day"]), left_cols=(0, 1, 2)))
        else:
            body.append(h3(f"No team flipped on {short_day(day)}"))
        if ou["flips_week"]:
            body.append(h3(f"Teams that flipped in the last 7 days (since {short_day(dt.date.fromisoformat(ou['week']))})"))
            body.append(table(["Team", "Status", "Points swing by entry"], flip_rows(ou["flips_week"]), left_cols=(0, 1, 2)))
        if ou["clinched"]:
            body.append(h3("Clinched"))
            body.append(para("; ".join(f"{e(c['team'])} clinched {ou_word(c['outcome'])}" for c in ou["clinched"]) + "."))
        parts.append(section(f'<a href="{OU_URL}" style="color:{HEAD}">Over/under pool</a>', "".join(body),
                             f"Through the games of {D}"))

        # ---- teams
        body = []
        if ou["last_night"]:
            rows = []
            for g in ou["last_night"]:
                aw = g["away_pts"] > g["home_pts"]
                a = f"<b>{e(g['away'])}</b>" if aw else e(g["away"])
                h = e(g["home"]) if aw else f"<b>{e(g['home'])}</b>"
                rows.append([f"{a} at {h}", f"{g['away_pts']}–{g['home_pts']}{' (OT)' if g['ot'] else ''}", g["margin"]])
            body.append(h3(f"Scores, {D}"))
            body.append(table(["Game", "Score", "Margin"], rows))
            top_score = max(((t, p) for g in ou["last_night"] for t, p in ((g["away"], g["away_pts"]), (g["home"], g["home_pts"]))), key=lambda x: x[1])
            top3 = max(((t, n) for g in ou["last_night"] for t, n in g["fg3m"].items()), key=lambda x: x[1])
            body.append(para(f"Most points: {e(top_score[0])}, {top_score[1]}. Most threes: {e(top3[0])}, {top3[1]}."))
        else:
            body.append(para(f"No NBA games were played on {D}."))
        wt = ou["week_teams"]
        if wt:
            def trow(x):
                return [e(x["team"]), f"{x['w']}–{x['l']}", signed(x["pm_g"], 1)]
            body.append(h3(f"Last 7 days ({short_day(dt.date.fromisoformat(ou['week_from']))} to {short_day(day)})"))
            body.append(para("<b>Hottest</b>"))
            body.append(table(["Team", "Record", "Margin a game"], [trow(x) for x in wt[:5]]))
            body.append(para("<b>Coldest</b>"))
            body.append(table(["Team", "Record", "Margin a game"], [trow(x) for x in wt[::-1][:5]]))
        parts.append(section("NBA teams", "".join(body)))

    # ---- fantasy
    if fa:
        body = []
        if not fa["games_day"]:
            body.append(para(f"No new fantasy point was saved for {D}, so the 1-day changes are blank."))
        rows = []
        for x in fa["table"]:
            rows.append([
                f"{x['place']}{place_move(x['place'], x['place_day'])}",
                f"<b>{e(x['manager'])}</b>",
                f"{x['strength']:.0f}",
                delta_cell(x["d_day"] if fa["games_day"] else None), delta_cell(x["d_week"]),
                f"{x['fp']:.1f}", delta_cell(x["fp_week"], 1),
            ])
        body.append(table(["#", "Manager", "Strength", "1 day", "7 days", "FPts/G", "7 days"], rows, left_cols=(0, 1)))
        body.append(para("<b>Strongest seasons:</b> " + "; ".join(
            f"{e(x['manager'])}, {e(x['best']['name'])} ({x['best']['strength']:.1f})" for x in fa["table"] if x["best"]) + "."))
        body.append(note("Strength, roster: the tracker's sum of player strength indexes (a roster of ten can score at most 1,000). "
                         "FPts/G: the roster's fantasy points a game. Both count only players past the minimum games."))
        if fa["night_top"]:
            body.append(h3(f"Best ballot games, {D}"))
            rows = [[f"<b>{e(r['name'].split(',')[0])}</b> <span style=\"color:{DIM}\">{e(r['team'])}</span>",
                     mgr_cell(r["manager"]), stat_line(r), f"{r['fp']:.1f}"] for r in fa["night_top"]]
            body.append(table(["Player", "Manager", "Line", "FPts"], rows, left_cols=(0, 1, 2)))
        if fa["week_top"]:
            body.append(h3("Best ballot players, last 7 days"))
            rows = [[f"<b>{e(r['name'].split(',')[0])}</b> <span style=\"color:{DIM}\">{e(r['team'])}</span>",
                     mgr_cell(r["manager"]), r["gp"],
                     f"{per_game(r, 'pts')}/{per_game(r, 'reb')}/{per_game(r, 'ast')}",
                     f"{r['fp'] / r['gp']:.1f}"] for r in fa["week_top"]]
            body.append(table(["Player", "Manager", "G", "Pts/Reb/Ast", "FPts/G"], rows, left_cols=(0, 1)))
            body.append(note("Ranked by fantasy points a game, at least two games."))
            body.append(h3("Fantasy points each roster produced, last 7 days"))
            rows = [[f"<b>{e(m['manager'])}</b>", m["games"], f"{m['fp']:.0f}"] for m in fa["mgr_week"]]
            body.append(table(["Manager", "Player games", "Fantasy points"], rows))
        parts.append(section(f'<a href="{FANTASY_PLAYERS_URL}" style="color:{HEAD}">Fantasy draft</a>', "".join(body),
                             f"Through the games of {long_day(dt.date.fromisoformat(fa['day']))}"))

    foot = note(
        f'Pages: <a href="{OU_URL}">over/under</a> and <a href="{FANTASY_URL}">fantasy draft</a>. '
        "Over/under numbers come from the 2:00 AM Mac mini build of the page. Fantasy numbers come from the "
        "tracker's saved daily ESPN points, refreshed at 4:00 AM Phoenix. Game lines for ballot players are the "
        "change in their season totals between saved days, so they cover the 90 ballot players only. "
        "Fantasy points: points, plus 1.2 a rebound, 1.5 an assist, 3 a steal, 3 a block, minus 1 a turnover.")

    title = f"NBA daily, {D}"
    return (f'<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width">'
            f"<title>{e(title)}</title></head>"
            f'<body style="margin:0;padding:0;background:{PAPER};color:{INK};font-family:Arial,Helvetica,sans-serif">'
            f'<div style="max-width:680px;margin:0 auto;padding:16px 12px">'
            f'<h1 style="margin:0 0 2px;font-family:Georgia,serif;font-size:24px;color:{HEAD}">NBA daily</h1>'
            f'<p style="margin:0 0 14px;color:{DIM};font-size:13px">Games of {e(D)}, {day.year}. Changes are against the day before and 7 days before.</p>'
            + "".join(parts) + foot + "</div></body></html>")


def subject(day, ou, fa):
    bits = [f"NBA daily, {short_day(day)}"]
    if ou:
        lead = ou["entries"][0]
        bits.append(f"O/U: {lead['entry']} {lead['points']:.0f}")
    if fa:
        bits.append(f"Fantasy: {fa['table'][0]['manager']} {fa['table'][0]['strength']:.0f}")
    return " · ".join(bits)


def main(argv=None):
    ap = argparse.ArgumentParser()
    ap.add_argument("run_dir")
    ap.add_argument("--date", help="the game day to summarize (default: yesterday, Phoenix)")
    ap.add_argument("--ou-dir", default=os.path.join(PROJECT, "check-export", "pool"))
    ap.add_argument("--chance", help="chance.csv (date, player, p_first, p_top4, exp_place)")
    ap.add_argument("--box", default=os.path.join(PROJECT, "current_year.csv"))
    ap.add_argument("--fantasy-dir", help="read players.json and history.json here instead of the site")
    a = ap.parse_args(argv)

    day = dt.date.fromisoformat(a.date) if a.date else dt.date.today() - dt.timedelta(days=1)
    chance = a.chance or os.path.join(a.run_dir, "chance.csv")
    os.makedirs(a.run_dir, exist_ok=True)
    problems = []
    try:
        ou = over_under(a.ou_dir, chance, a.box, day)
    except Exception as ex:   # one source down still sends the other
        ou = None
        problems.append(f"over/under: {type(ex).__name__}: {ex}")
    try:
        fa = fantasy(day, a.fantasy_dir)
    except Exception as ex:
        fa = None
        problems.append(f"fantasy: {type(ex).__name__}: {ex}")
    if ou is None and fa is None:
        print("\n".join(problems), file=sys.stderr)
        return 1
    page = render(day, ou, fa)
    if problems:
        page = page.replace("</body>", note("Not included: " + e("; ".join(problems))) + "</body>")
    with open(os.path.join(a.run_dir, "email.html"), "w", encoding="utf-8") as f:
        f.write(page)
    subj = subject(day, ou, fa)
    with open(os.path.join(a.run_dir, "subject.txt"), "w", encoding="utf-8") as f:
        f.write(subj + "\n")
    with open(os.path.join(a.run_dir, "summary.json"), "w", encoding="utf-8") as f:
        # games were played but the fantasy refresh has not saved that day yet
        behind = bool(ou and ou["last_night"]) and (fa is None or fa["day"] != iso(day))
        json.dump({"day": iso(day), "ou": ou, "fantasy": fa, "problems": problems,
                   "fantasy_behind": behind}, f, indent=1, default=str)
    for p in problems:
        print(p, file=sys.stderr)
    print(subj)
    return 0


if __name__ == "__main__":
    sys.exit(main())
