"""
06a-fetch-official-standings.py
Fetch the official NBA standings (with tiebreaker-applied playoff seeds)

Sources (tried in order):
  1. nba_api Python package -- LeagueStandings endpoint (PlayoffRank column)
  2. ESPN standings v2 API -- JSON with playoffSeed in stats
  3. ESPN HTML page -- teams listed in seed order, parsed with BeautifulSoup

Outputs: official_standings.csv
  Columns: seed, team_name, conference, wins, losses, source
"""

import pandas as pd
import requests
import re
import sys
import time
import json
from datetime import datetime

CSV_OUT = "official_standings.csv"

# ESPN displayName -> project meta sheet name mapping
ESPN_NAME_MAP = {
    "LA Clippers": "Los Angeles Clippers",
}

# =============================================================================
# SOURCE 1: nba_api package (LeagueStandings endpoint)
# This returns PlayoffRank -- the official seed with tiebreakers applied.
# =============================================================================

def try_nba_api_package():
    """Use the nba_api package to fetch standings with PlayoffRank."""
    print("=" * 60)
    print("SOURCE 1: nba_api package (LeagueStandings)")
    print("=" * 60)

    try:
        from nba_api.stats.endpoints import leaguestandings

        headers = {
            'Host': 'stats.nba.com',
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36',
            'Accept': 'application/json, text/plain, */*',
            'Referer': 'https://www.nba.com/',
            'Origin': 'https://www.nba.com'
        }

        print("  Fetching (timeout=120s)...")
        standings = leaguestandings.LeagueStandings(
            season="2025-26",
            headers=headers,
            timeout=120
        )
        df = standings.get_data_frames()[0]

        result = df[["PlayoffRank", "TeamCity", "TeamName",
                      "Conference", "WINS", "LOSSES"]].copy()
        result["team_name"] = (result["TeamCity"] + " " + result["TeamName"]).replace(ESPN_NAME_MAP)
        result = result.rename(columns={
            "PlayoffRank": "seed",
            "Conference": "conference",
            "WINS": "wins",
            "LOSSES": "losses",
        })
        result["seed"] = result["seed"].astype(int)
        result["source"] = "nba.com"
        result = result[["seed", "team_name", "conference", "wins",
                         "losses", "source"]]
        result = result.sort_values(["conference", "seed"]).reset_index(drop=True)

        print(f"  SUCCESS -- got {len(result)} teams")
        return result

    except ImportError:
        print("  nba_api package not installed, skipping")
        return None
    except Exception as e:
        print(f"  FAILED: {e}")
        return None


# =============================================================================
# SOURCE 2: ESPN standings v2 API (JSON)
# =============================================================================

def try_espn_api():
    """
    Fetch from ESPN's standings API.
    Handle the response structure defensively since it is undocumented.
    """
    print("=" * 60)
    print("SOURCE 2: ESPN v2 standings API")
    print("=" * 60)

    url = "https://site.api.espn.com/apis/v2/sports/basketball/nba/standings"
    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
            "AppleWebKit/537.36"
        ),
    }

    try:
        resp = requests.get(url, headers=headers, timeout=30)
        resp.raise_for_status()
        data = resp.json()

        records = []
        children = data.get("children", [])
        if not children:
            print("  No 'children' key in response")
            return None

        for child in children:
            conf_name = child.get("name", child.get("abbreviation", ""))
            conf = "East" if "East" in conf_name else "West" if "West" in conf_name else conf_name

            entries = child.get("standings", {}).get("entries", [])

            for idx, entry in enumerate(entries):
                team_info = entry.get("team", {})
                display_name = team_info.get("displayName", "")
                team_name = ESPN_NAME_MAP.get(display_name, display_name)

                # Extract stats -- handle multiple possible structures
                raw_stats = entry.get("stats", [])
                stats_dict = {}
                for s in raw_stats:
                    name = s.get("name", s.get("abbreviation", ""))
                    # Try multiple possible value keys
                    val = (s.get("value") if "value" in s
                           else s.get("displayValue") if "displayValue" in s
                           else s.get("summary") if "summary" in s
                           else None)
                    if name and val is not None:
                        stats_dict[name] = val

                # Try to find wins/losses from various possible stat names
                wins = None
                losses = None

                # Try direct stats
                for wk in ["wins", "Wins", "WINS", "W", "w"]:
                    if wk in stats_dict:
                        try:
                            wins = int(float(stats_dict[wk]))
                        except (ValueError, TypeError):
                            pass
                        break

                for lk in ["losses", "Losses", "LOSSES", "L", "l"]:
                    if lk in stats_dict:
                        try:
                            losses = int(float(stats_dict[lk]))
                        except (ValueError, TypeError):
                            pass
                        break

                # Try parsing from "overall" record if W/L not directly available
                if wins is None or losses is None:
                    for rk in ["overall", "record", "clincher"]:
                        if rk in stats_dict:
                            m = re.match(r"(\d+)-(\d+)", str(stats_dict[rk]))
                            if m:
                                wins = int(m.group(1))
                                losses = int(m.group(2))
                                break

                # Try to get seed
                seed = None
                for sk in ["playoffSeed", "seed", "rank"]:
                    if sk in stats_dict:
                        try:
                            seed = int(float(stats_dict[sk]))
                        except (ValueError, TypeError):
                            pass
                        break

                # Fallback: use position in list (entries come in seed order)
                if seed is None:
                    seed = idx + 1

                records.append({
                    "seed": seed,
                    "team_name": team_name,
                    "conference": conf,
                    "wins": wins,
                    "losses": losses,
                    "source": "espn_api",
                })

        if records:
            df = pd.DataFrame(records).sort_values(["conference", "seed"]).reset_index(drop=True)

            # Validate we have 30 teams
            if len(df) != 30:
                print(f"  Got {len(df)} teams, expected 30")
                return None

            # Check if we have W-L
            if df["wins"].isna().any():
                print("  WARNING: Some W-L records are missing")
                # Still usable for seed comparison if seeds are present

            print(f"  SUCCESS -- got {len(df)} teams")
            return df

    except Exception as e:
        print(f"  FAILED: {e}")
        import traceback
        traceback.print_exc()

    return None


# =============================================================================
# SOURCE 3: ESPN HTML page parsed with BeautifulSoup
# =============================================================================

def try_espn_html():
    """
    Parse ESPN's NBA standings HTML page.
    Teams appear in seed order within each conference.
    """
    print("=" * 60)
    print("SOURCE 3: ESPN HTML standings page")
    print("=" * 60)

    try:
        from bs4 import BeautifulSoup
    except ImportError:
        print("  BeautifulSoup not available, skipping")
        return None

    url = "https://www.espn.com/nba/standings"
    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/120.0.0.0 Safari/537.36"
        ),
    }

    try:
        resp = requests.get(url, headers=headers, timeout=30)
        resp.raise_for_status()
        soup = BeautifulSoup(resp.text, "html.parser")

        # The page has two sections (East, West).
        # Team names appear as links: /nba/team/_/name/{abbr}/{slug}
        # Stats appear in separate aligned table bodies.

        # Find all team link elements -- they contain the display name
        team_links = soup.find_all("a", href=re.compile(r"/nba/team/_/name/"))

        # Extract unique team display names in order of appearance
        seen = set()
        ordered_teams = []
        for link in team_links:
            name = link.get_text(strip=True)
            # Filter to actual team names (skip short abbreviations)
            if len(name) > 3 and name not in seen:
                seen.add(name)
                ordered_teams.append(name)

        if len(ordered_teams) < 30:
            print(f"  Only found {len(ordered_teams)} teams from links")
            return None

        # The first 15 are East (in seed order), next 15 are West
        east_teams = ordered_teams[:15]
        west_teams = ordered_teams[15:30]

        # Find all table bodies with stats
        # Look for <tbody> elements that contain numeric cells (W, L)
        all_tbodies = soup.find_all("tbody", class_=re.compile("Table__TBODY"))

        # ESPN renders paired tbody: one for team names, one for stats
        # The stats tbody has rows with W, L, PCT, GB, etc.
        # We need to find the stats tbodies and extract W and L.

        def extract_wl_from_tbody(tbody):
            """Extract (wins, losses) pairs from a stats table body."""
            pairs = []
            rows = tbody.find_all("tr")
            for row in rows:
                cells = row.find_all("td")
                if len(cells) >= 2:
                    try:
                        w = int(cells[0].get_text(strip=True))
                        l = int(cells[1].get_text(strip=True))
                        pairs.append((w, l))
                    except (ValueError, IndexError):
                        continue
            return pairs

        # Try to find stats tbodies
        # The pattern is: name-tbody, stats-tbody, name-tbody, stats-tbody
        stat_tbodies = []
        for tbody in all_tbodies:
            pairs = extract_wl_from_tbody(tbody)
            if len(pairs) == 15:  # full conference
                stat_tbodies.append(pairs)

        # Build records
        records = []

        east_wl = stat_tbodies[0] if len(stat_tbodies) >= 1 else [(None, None)] * 15
        west_wl = stat_tbodies[1] if len(stat_tbodies) >= 2 else [(None, None)] * 15

        for i, team in enumerate(east_teams):
            team_name = ESPN_NAME_MAP.get(team, team)
            w, l = east_wl[i] if i < len(east_wl) else (None, None)
            records.append({
                "seed": i + 1,
                "team_name": team_name,
                "conference": "East",
                "wins": w,
                "losses": l,
                "source": "espn_html",
            })

        for i, team in enumerate(west_teams):
            team_name = ESPN_NAME_MAP.get(team, team)
            w, l = west_wl[i] if i < len(west_wl) else (None, None)
            records.append({
                "seed": i + 1,
                "team_name": team_name,
                "conference": "West",
                "wins": w,
                "losses": l,
                "source": "espn_html",
            })

        df = pd.DataFrame(records)
        print(f"  SUCCESS -- got {len(df)} teams")
        return df

    except Exception as e:
        print(f"  FAILED: {e}")
        import traceback
        traceback.print_exc()
        return None


# =============================================================================
# MAIN
# =============================================================================

def main():
    print(f"\nFetching official NBA standings at {datetime.now()}\n")

    # Source 1: nba_api package
    df = try_nba_api_package()

    # Source 2: ESPN API
    if df is None:
        print()
        df = try_espn_api()

    # Source 3: ESPN HTML
    if df is None:
        print()
        df = try_espn_html()

    if df is None or len(df) == 0:
        print("\nAll sources failed!")
        sys.exit(1)

    df.to_csv(CSV_OUT, index=False)
    print(f"\nSaved to {CSV_OUT}")

    # Print summary
    for conf in ["East", "West"]:
        print(f"\n{'=' * 50}")
        print(f"  {conf}ern Conference")
        print(f"{'=' * 50}")
        conf_df = df[df["conference"] == conf].sort_values("seed")
        for _, row in conf_df.iterrows():
            marker = ""
            if row["seed"] == 6:
                marker = "  <-- playoff cutoff"
            elif row["seed"] == 10:
                marker = "  <-- play-in cutoff"
            print(
                f"  {row['seed']:>2}. {row['team_name']:<30} "
                f"{row['wins']:>2}-{row['losses']:<2}{marker}"
            )

    print(f"\nSource: {df['source'].iloc[0]}")


if __name__ == "__main__":
    main()
