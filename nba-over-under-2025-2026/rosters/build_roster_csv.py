#!/usr/bin/env python3
"""Pull current NBA rosters and write finder_app/players_finder.csv.

Run weekly by .github/workflows/nba-player-finder.yaml. Uses the nba_api
CommonTeamRoster endpoint (the same data source already proven on GitHub
Actions by nba-over-under.yaml) and joins conference/division/abbreviation
from team_meta.csv (exported once from picks.xlsx).

Output columns match what finder_app/app.R filters on:
    player, team, conference, division,
    height_feet, height_inches, height_total_inches,
    age, number_jersey, date_pulled

Safety guard: if fewer than MIN_PLAYERS are pulled (e.g. the API blocked the
runner), the script exits non-zero WITHOUT writing, so a bad pull never wipes
the last good roster that is committed to the repo.
"""

import os
import sys
import time
import random
from datetime import date

import pandas as pd
from nba_api.stats.static import teams as static_teams
from nba_api.stats.endpoints import commonteamroster

# The season is derived automatically from today's date (see candidate_seasons).
# Set the NBA_SEASON env var (e.g. "2025-26") to force a specific season instead.
SEASON_OVERRIDE = os.environ.get("NBA_SEASON")

# Month (1-12) from which we start *trying* the upcoming season. NBA training
# camps open in late September, so from September on we attempt the new season
# first and fall back to the prior one until its rosters are actually posted.
SEASON_ROLLOVER_MONTH = 9

# Minimum sane player count; below this we assume a blocked/failed/too-early pull.
MIN_PLAYERS = 400

# Browser-like headers (mirrors 02b-get_game_scores.py) so stats.nba.com is
# less likely to block the request.
HEADERS = {
    "Host": "stats.nba.com",
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
    "Accept": "application/json, text/plain, */*",
    "Referer": "https://www.nba.com/",
    "Origin": "https://www.nba.com",
}

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
TEAM_META_PATH = os.path.join(SCRIPT_DIR, "team_meta.csv")
OUTPUT_PATH = os.path.join(SCRIPT_DIR, "finder_app", "players_finder.csv")


def season_string(start_year):
    """2025 -> '2025-26'."""
    return f"{start_year}-{str(start_year + 1)[-2:]}"


def candidate_seasons(today=None):
    """Seasons to try, newest first.

    NBA seasons span Oct-June and are labeled by their starting year (the
    2025-26 season starts in Oct 2025). From SEASON_ROLLOVER_MONTH (September)
    onward we try the upcoming season first but keep the prior season as a
    fallback, so the switch happens automatically once the new rosters are
    posted -- and never leaves us empty-handed during the early-September gap.
    """
    if today is None:
        today = date.today()
    start_year = today.year if today.month >= SEASON_ROLLOVER_MONTH else today.year - 1
    seasons = [season_string(start_year)]
    prev = season_string(start_year - 1)
    if prev not in seasons:
        seasons.append(prev)
    return seasons


def fetch_team_roster(team_id, team_name, season, max_attempts=4):
    """Fetch one team's roster, retrying with backoff on failure."""
    for attempt in range(1, max_attempts + 1):
        try:
            roster = commonteamroster.CommonTeamRoster(
                team_id=team_id,
                season=season,
                headers=HEADERS,
                timeout=60,
            )
            df = roster.get_data_frames()[0]
            if len(df) > 0:
                df = df.copy()
                df["team"] = team_name
                return df
            raise ValueError("empty roster")
        except Exception as exc:  # noqa: BLE001 - retry on any error
            print(f"  {team_name}: attempt {attempt}/{max_attempts} failed: {exc}")
            time.sleep(attempt * 3 + random.uniform(0, 2))
    print(f"  {team_name}: giving up after {max_attempts} attempts")
    return None


def split_height(value):
    """'6-7' -> (6, 7, 79). Returns (None, None, None) when unparseable."""
    if not isinstance(value, str) or "-" not in value:
        return (None, None, None)
    feet_str, inches_str = value.split("-", 1)
    try:
        feet = int(feet_str)
        inches = int(inches_str)
    except ValueError:
        return (None, None, None)
    return (feet, inches, feet * 12 + inches)


def season_is_available(season):
    """Fast probe: does at least one team have a populated roster for this season?

    Avoids a slow 30-team pull (with per-team retries) against a season whose
    rosters aren't posted yet, e.g. the upcoming season in early September.
    """
    sample = static_teams.get_teams()[0]
    df = fetch_team_roster(sample["id"], sample["full_name"], season, max_attempts=2)
    return df is not None and len(df) > 0


def build_for_season(season, meta):
    """Pull every team's roster for one season and return the finder DataFrame
    (or None if nothing came back)."""
    frames = []
    for team in static_teams.get_teams():
        df = fetch_team_roster(team["id"], team["full_name"], season)
        if df is not None:
            frames.append(df)
        time.sleep(1.5)  # be polite to the API

    if not frames:
        return None

    raw = pd.concat(frames, ignore_index=True)

    heights = raw["HEIGHT"].apply(split_height)
    out = pd.DataFrame(
        {
            "player": raw["PLAYER"],
            "team": raw["team"],
            "height_feet": [h[0] for h in heights],
            "height_inches": [h[1] for h in heights],
            "height_total_inches": [h[2] for h in heights],
            "age": pd.to_numeric(raw["AGE"], errors="coerce"),
            "number_jersey": raw["NUM"],
        }
    )

    out = out.merge(meta[["team", "conference", "division"]], on="team", how="inner")
    out["date_pulled"] = date.today().isoformat()

    # Match app column order: player, team, conference, division, heights, age, jersey, date
    return out[
        [
            "player",
            "team",
            "conference",
            "division",
            "height_feet",
            "height_inches",
            "height_total_inches",
            "age",
            "number_jersey",
            "date_pulled",
        ]
    ]


def main():
    meta = pd.read_csv(TEAM_META_PATH)

    seasons = [SEASON_OVERRIDE] if SEASON_OVERRIDE else candidate_seasons()
    print(f"Season candidates (newest first): {seasons}")

    out = None
    chosen = None
    for season in seasons:
        # Skip a clearly-unavailable season fast, unless the user forced one.
        if not SEASON_OVERRIDE and not season_is_available(season):
            print(f"Season {season} not posted yet; trying fallback ...")
            continue

        print(f"Pulling rosters for season {season} ...")
        candidate = build_for_season(season, meta)
        n = 0 if candidate is None else len(candidate)
        if candidate is not None and n >= MIN_PLAYERS:
            out, chosen = candidate, season
            break
        print(f"  season {season}: only {n} players (< {MIN_PLAYERS}); trying fallback ...")

    if out is None:
        print(
            f"ERROR: no candidate season produced >= {MIN_PLAYERS} players; "
            "refusing to overwrite the last good roster.",
            file=sys.stderr,
        )
        sys.exit(1)

    print(f"Pulled {len(out)} players across {out['team'].nunique()} teams for {chosen}.")

    os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
    out.to_csv(OUTPUT_PATH, index=False)
    print(f"Wrote {OUTPUT_PATH} (season {chosen})")


if __name__ == "__main__":
    main()
