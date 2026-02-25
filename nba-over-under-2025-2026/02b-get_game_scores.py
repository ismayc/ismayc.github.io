import pandas as pd
import requests
from bs4 import BeautifulSoup
from datetime import datetime, timedelta
import time
import random
import sys
import os

CSV_PATH = "current_year.csv"
YEAR = '2025'

EXPECTED_COLS = [
    'SEASON_ID', 'TEAM_ID', 'TEAM_ABBREVIATION', 'TEAM_NAME', 'GAME_ID',
    'GAME_DATE', 'MATCHUP', 'WL', 'MIN', 'PTS', 'FGM', 'FGA', 'FG_PCT',
    'FG3M', 'FG3A', 'FG3_PCT', 'FTM', 'FTA', 'FT_PCT', 'OREB', 'DREB',
    'REB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PLUS_MINUS'
]

def get_last_game_date():
    """Get the most recent game date from existing CSV."""
    if os.path.exists(CSV_PATH):
        try:
            df = pd.read_csv(CSV_PATH)
            if 'GAME_DATE' in df.columns and len(df) > 0:
                last_date = pd.to_datetime(df['GAME_DATE']).max()
                return last_date.to_pydatetime()
        except Exception:
            pass
    return None

# =============================================================================
# OPTION 1: NBA API
# =============================================================================

def try_nba_api():
    """Try NBA API with single quick attempt."""
    print("=" * 60)
    print("ATTEMPT 1: NBA API (stats.nba.com)")
    print("=" * 60)
    
    try:
        from nba_api.stats.endpoints import leaguegamefinder
        
        headers = {
            'Host': 'stats.nba.com',
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36',
            'Accept': 'application/json, text/plain, */*',
            'Referer': 'https://www.nba.com/',
            'Origin': 'https://www.nba.com'
        }
        
        print("  Attempting with 60s timeout...")
        gamefinder = leaguegamefinder.LeagueGameFinder(
            headers=headers,
            timeout=60
        )
        games = gamefinder.get_data_frames()[0]
        season_games = games[games.SEASON_ID.str[-4:] == YEAR]
        
        if len(season_games) > 0:
            print(f"  SUCCESS! Got {len(season_games)} records")
            return season_games
            
    except Exception as e:
        print(f"  FAILED: {e}")
    
    return None

# =============================================================================
# OPTION 2: ESPN API (incremental update)
# =============================================================================

def fetch_espn_scoreboard(date_str):
    """Fetch scoreboard from ESPN API."""
    url = "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard"
    response = requests.get(url, params={"dates": date_str}, timeout=15)
    response.raise_for_status()
    return response.json()

def fetch_espn_boxscore(game_id):
    """Fetch detailed box score."""
    url = "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/summary"
    response = requests.get(url, params={"event": game_id}, timeout=15)
    response.raise_for_status()
    return response.json()

def extract_espn_team_stats(boxscore_data, team_id):
    """Extract team stats from ESPN box score."""
    stats = {}
    try:
        teams = boxscore_data.get('boxscore', {}).get('teams', [])
        for team in teams:
            if str(team.get('team', {}).get('id')) == str(team_id):
                for stat in team.get('statistics', []):
                    name, value = stat.get('name'), stat.get('displayValue')
                    mapping = {
                        'fieldGoalsMade': 'FGM', 'fieldGoalsAttempted': 'FGA',
                        'fieldGoalPct': 'FG_PCT', 'threePointFieldGoalsMade': 'FG3M',
                        'threePointFieldGoalsAttempted': 'FG3A', 'threePointFieldGoalPct': 'FG3_PCT',
                        'freeThrowsMade': 'FTM', 'freeThrowsAttempted': 'FTA',
                        'freeThrowPct': 'FT_PCT', 'totalRebounds': 'REB',
                        'offensiveRebounds': 'OREB', 'defensiveRebounds': 'DREB',
                        'assists': 'AST', 'steals': 'STL', 'blocks': 'BLK',
                        'turnovers': 'TOV', 'fouls': 'PF',
                    }
                    if name in mapping and value:
                        try:
                            stats[mapping[name]] = float(value) if 'Pct' in name else int(float(value))
                        except (ValueError, TypeError):
                            pass
                break
    except Exception:
        pass
    return stats

def try_espn_api(start_date=None):
    """Fetch games from ESPN, starting from start_date."""
    print("=" * 60)
    print("ATTEMPT 2: ESPN API")
    print("=" * 60)
    
    if start_date is None:
        start_date = datetime(2025, 10, 21)
    
    # End date is TODAY (to catch yesterday's late games that post after midnight)
    end_date = datetime.now()
    
    # Make sure we go back far enough to catch any missed games
    # Go back 3 days from last known date to be safe
    if start_date > datetime(2025, 10, 21):
        start_date = start_date - timedelta(days=3)
    
    total_days = (end_date - start_date).days + 1
    
    print(f"  Fetching from {start_date.strftime('%Y-%m-%d')} to {end_date.strftime('%Y-%m-%d')} ({total_days} days)")
    
    all_records = []
    current_date = start_date
    day_count = 0
    
    while current_date <= end_date:
        day_count += 1
        date_str = current_date.strftime('%Y%m%d')
        
        try:
            data = fetch_espn_scoreboard(date_str)
            
            for game in data.get('events', []):
                game_id = game.get('id')
                game_date = game.get('date', '')[:10]
                
                competitions = game.get('competitions', [])
                if not competitions:
                    continue
                
                comp = competitions[0]
                if not comp.get('status', {}).get('type', {}).get('completed', False):
                    continue
                
                competitors = comp.get('competitors', [])
                if len(competitors) != 2:
                    continue
                
                home = away = None
                for c in competitors:
                    if c.get('homeAway') == 'home':
                        home = c
                    else:
                        away = c
                
                if not home or not away:
                    continue
                
                # Fetch box score
                home_stats, away_stats = {}, {}
                try:
                    time.sleep(0.1)
                    box = fetch_espn_boxscore(game_id)
                    home_stats = extract_espn_team_stats(box, home['team']['id'])
                    away_stats = extract_espn_team_stats(box, away['team']['id'])
                except Exception:
                    pass
                
                h_info, a_info = home['team'], away['team']
                h_score, a_score = int(home['score']), int(away['score'])
                h_abbr, a_abbr = h_info['abbreviation'], a_info['abbreviation']
                
                home_stats['PLUS_MINUS'] = float(h_score - a_score)
                away_stats['PLUS_MINUS'] = float(a_score - h_score)
                
                all_records.append({
                    'SEASON_ID': '22025', 'TEAM_ID': h_info['id'],
                    'TEAM_ABBREVIATION': h_abbr, 'TEAM_NAME': h_info['displayName'],
                    'GAME_ID': game_id, 'GAME_DATE': game_date,
                    'MATCHUP': f"{h_abbr} vs. {a_abbr}",
                    'WL': 'W' if h_score > a_score else 'L',
                    'PTS': h_score, **home_stats
                })
                
                all_records.append({
                    'SEASON_ID': '22025', 'TEAM_ID': a_info['id'],
                    'TEAM_ABBREVIATION': a_abbr, 'TEAM_NAME': a_info['displayName'],
                    'GAME_ID': game_id, 'GAME_DATE': game_date,
                    'MATCHUP': f"{a_abbr} @ {h_abbr}",
                    'WL': 'W' if a_score > h_score else 'L',
                    'PTS': a_score, **away_stats
                })
            
            time.sleep(0.15)
            
        except KeyboardInterrupt:
            raise
        except Exception as e:
            print(f"  Error on {date_str}: {e}")
            time.sleep(1)
        
        current_date += timedelta(days=1)
        
        # Progress every 10 days
        if day_count % 10 == 0:
            print(f"  Progress: {day_count}/{total_days} days")
    
    if all_records:
        print(f"  SUCCESS! Got {len(all_records)} records")
        return pd.DataFrame(all_records)
    
    return None

# =============================================================================
# OPTION 3: Basketball Reference (incremental)
# =============================================================================

TEAM_ABBREV = {
    'Atlanta Hawks': 'ATL', 'Boston Celtics': 'BOS', 'Brooklyn Nets': 'BKN',
    'Charlotte Hornets': 'CHA', 'Chicago Bulls': 'CHI', 'Cleveland Cavaliers': 'CLE',
    'Dallas Mavericks': 'DAL', 'Denver Nuggets': 'DEN', 'Detroit Pistons': 'DET',
    'Golden State Warriors': 'GSW', 'Houston Rockets': 'HOU', 'Indiana Pacers': 'IND',
    'Los Angeles Clippers': 'LAC', 'Los Angeles Lakers': 'LAL', 'Memphis Grizzlies': 'MEM',
    'Miami Heat': 'MIA', 'Milwaukee Bucks': 'MIL', 'Minnesota Timberwolves': 'MIN',
    'New Orleans Pelicans': 'NOP', 'New York Knicks': 'NYK', 'Oklahoma City Thunder': 'OKC',
    'Orlando Magic': 'ORL', 'Philadelphia 76ers': 'PHI', 'Phoenix Suns': 'PHX',
    'Portland Trail Blazers': 'POR', 'Sacramento Kings': 'SAC', 'San Antonio Spurs': 'SAS',
    'Toronto Raptors': 'TOR', 'Utah Jazz': 'UTA', 'Washington Wizards': 'WAS',
}

BREF_HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36',
    'Accept': 'text/html,application/xhtml+xml',
    'Referer': 'https://www.google.com/',
}

def fetch_bref_boxscore(game_id):
    """Fetch box score from Basketball Reference."""
    url = f"https://www.basketball-reference.com/boxscores/{game_id}.html"
    time.sleep(random.uniform(3, 5))
    response = requests.get(url, headers=BREF_HEADERS, timeout=30)
    response.raise_for_status()
    
    soup = BeautifulSoup(response.text, 'html.parser')
    team_stats = {}
    
    for table in soup.find_all('table'):
        tid = table.get('id', '')
        if '_basic' in tid and tid.startswith('box-'):
            abbr = tid.replace('box-', '').replace('-game-basic', '').upper()
            abbr = {'BRK': 'BKN', 'CHO': 'CHA', 'PHO': 'PHX'}.get(abbr, abbr)
            
            tfoot = table.find('tfoot')
            if tfoot:
                row = tfoot.find('tr')
                if row:
                    stats = {}
                    for cell in row.find_all(['th', 'td']):
                        name, val = cell.get('data-stat', ''), cell.get_text(strip=True)
                        mapping = {
                            'fg': 'FGM', 'fga': 'FGA', 'fg_pct': 'FG_PCT',
                            'fg3': 'FG3M', 'fg3a': 'FG3A', 'fg3_pct': 'FG3_PCT',
                            'ft': 'FTM', 'fta': 'FTA', 'ft_pct': 'FT_PCT',
                            'orb': 'OREB', 'drb': 'DREB', 'trb': 'REB',
                            'ast': 'AST', 'stl': 'STL', 'blk': 'BLK',
                            'tov': 'TOV', 'pf': 'PF',
                        }
                        if name in mapping and val:
                            try:
                                stats[mapping[name]] = float(val) if 'pct' in name else int(val)
                            except (ValueError, TypeError):
                                pass
                    team_stats[abbr] = stats
    
    return team_stats

def try_bref(start_date=None):
    """Fetch from Basketball Reference."""
    print("=" * 60)
    print("ATTEMPT 3: Basketball Reference")
    print("=" * 60)
    
    # Go back 3 days to catch missed games
    if start_date and start_date > datetime(2025, 10, 21):
        start_date = start_date - timedelta(days=3)
    
    # Determine which months to fetch
    if start_date:
        start_month = start_date.month
    else:
        start_month = 10  # October
    
    month_names = {10: 'october', 11: 'november', 12: 'december',
                   1: 'january', 2: 'february', 3: 'march', 4: 'april'}
    
    current_month = datetime.now().month
    months_to_fetch = []
    
    # Build list of months from start to current
    m = start_month
    while True:
        if m in month_names:
            months_to_fetch.append(month_names[m])
        if m == current_month:
            break
        m = 1 if m == 12 else m + 1
    
    all_records = []
    
    for month in months_to_fetch:
        try:
            print(f"  Fetching {month}...")
            url = f"https://www.basketball-reference.com/leagues/NBA_2026_games-{month}.html"
            
            time.sleep(random.uniform(2, 4))
            resp = requests.get(url, headers=BREF_HEADERS, timeout=30)
            
            if resp.status_code == 404:
                continue
            resp.raise_for_status()
            
            soup = BeautifulSoup(resp.text, 'html.parser')
            table = soup.find('table', {'id': 'schedule'})
            if not table:
                continue
            
            games = []
            for row in table.find('tbody').find_all('tr'):
                if row.get('class') and 'thead' in row.get('class'):
                    continue
                
                cells = row.find_all(['th', 'td'])
                if len(cells) < 6:
                    continue
                
                try:
                    date_str = cells[0].get_text(strip=True)
                    visitor = cells[2].get_text(strip=True)
                    v_pts = cells[3].get_text(strip=True)
                    home = cells[4].get_text(strip=True)
                    h_pts = cells[5].get_text(strip=True)
                    
                    if not v_pts or not h_pts:
                        continue
                    
                    game_date = datetime.strptime(date_str, '%a, %b %d, %Y')
                    
                    # Skip if before start_date
                    if start_date and game_date < start_date:
                        continue
                    
                    game_id = None
                    for cell in cells:
                        link = cell.find('a', href=lambda x: x and 'boxscores' in str(x))
                        if link:
                            game_id = link.get('href', '').split('/')[-1].replace('.html', '')
                            break
                    
                    if game_id:
                        games.append({
                            'game_id': game_id, 'game_date': game_date,
                            'visitor': visitor, 'v_pts': int(v_pts),
                            'home': home, 'h_pts': int(h_pts),
                        })
                except (ValueError, IndexError):
                    continue
            
            print(f"    Found {len(games)} games, fetching box scores...")
            
            for i, g in enumerate(games):
                try:
                    box = fetch_bref_boxscore(g['game_id'])
                    
                    v_abbr = TEAM_ABBREV.get(g['visitor'], g['visitor'][:3].upper())
                    h_abbr = TEAM_ABBREV.get(g['home'], g['home'][:3].upper())
                    
                    v_stats = box.get(v_abbr, {})
                    h_stats = box.get(h_abbr, {})
                    
                    v_stats['PLUS_MINUS'] = float(g['v_pts'] - g['h_pts'])
                    h_stats['PLUS_MINUS'] = float(g['h_pts'] - g['v_pts'])
                    
                    all_records.append({
                        'SEASON_ID': '22025', 'TEAM_ID': None,
                        'TEAM_ABBREVIATION': v_abbr, 'TEAM_NAME': g['visitor'],
                        'GAME_ID': g['game_id'],
                        'GAME_DATE': g['game_date'].strftime('%Y-%m-%d'),
                        'MATCHUP': f"{v_abbr} @ {h_abbr}",
                        'WL': 'W' if g['v_pts'] > g['h_pts'] else 'L',
                        'PTS': g['v_pts'], **v_stats
                    })
                    
                    all_records.append({
                        'SEASON_ID': '22025', 'TEAM_ID': None,
                        'TEAM_ABBREVIATION': h_abbr, 'TEAM_NAME': g['home'],
                        'GAME_ID': g['game_id'],
                        'GAME_DATE': g['game_date'].strftime('%Y-%m-%d'),
                        'MATCHUP': f"{h_abbr} vs. {v_abbr}",
                        'WL': 'W' if g['h_pts'] > g['v_pts'] else 'L',
                        'PTS': g['h_pts'], **h_stats
                    })
                    
                    if (i + 1) % 10 == 0:
                        print(f"      {i+1}/{len(games)} games")
                        
                except Exception as e:
                    print(f"      Error on {g['game_id']}: {e}")
                    
        except Exception as e:
            print(f"    Error: {e}")
    
    if all_records:
        print(f"  SUCCESS! Got {len(all_records)} records")
        return pd.DataFrame(all_records)
    
    return None

# =============================================================================
# MAIN
# =============================================================================

def format_df(df):
    """Ensure all columns exist and are ordered correctly."""
    for col in EXPECTED_COLS:
        if col not in df.columns:
            df[col] = None
    return df[[c for c in EXPECTED_COLS if c in df.columns]].sort_values('GAME_DATE', ascending=False)

def main():
    print(f"\nStarted at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
    
    # Check for existing data
    existing_df = None
    last_date = get_last_game_date()
    
    if last_date:
        print(f"Existing data found. Last game date: {last_date.strftime('%Y-%m-%d')}")
        print(f"Today's date: {datetime.now().strftime('%Y-%m-%d')}")
        try:
            existing_df = pd.read_csv(CSV_PATH)
            print(f"Existing records: {len(existing_df)}")
        except Exception:
            existing_df = None
    
    df = None
    
    # Try NBA API first (fastest if it works)
    df = try_nba_api()
    
    # Try ESPN if NBA API failed
    if df is None:
        print()
        # If we have existing data, only fetch new games (with 3-day overlap)
        if existing_df is not None and last_date:
            print(f"Incremental update starting from {(last_date - timedelta(days=3)).strftime('%Y-%m-%d')}")
            new_df = try_espn_api(start_date=last_date)
            
            if new_df is not None and len(new_df) > 0:
                # Merge with existing, removing duplicates
                combined = pd.concat([existing_df, new_df], ignore_index=True)
                # Convert GAME_ID to string for consistent comparison
                combined['GAME_ID'] = combined['GAME_ID'].astype(str)
                combined = combined.drop_duplicates(
                    subset=['GAME_ID', 'TEAM_ABBREVIATION'], 
                    keep='last'
                )
                df = combined
                print(f"  Combined: {len(existing_df)} existing + {len(new_df)} new = {len(df)} total (after dedup)")
        else:
            df = try_espn_api()
    
    # Try Basketball Reference as last resort
    if df is None:
        print()
        if existing_df is not None and last_date:
            new_df = try_bref(start_date=last_date)
            
            if new_df is not None and len(new_df) > 0:
                combined = pd.concat([existing_df, new_df], ignore_index=True)
                combined['GAME_ID'] = combined['GAME_ID'].astype(str)
                combined = combined.drop_duplicates(
                    subset=['GAME_ID', 'TEAM_ABBREVIATION'],
                    keep='last'
                )
                df = combined
        else:
            df = try_bref()
    
    # Handle failure
    if df is None or len(df) == 0:
        print("\n" + "=" * 60)
        print("ALL SOURCES FAILED!")
        print("=" * 60)
        
        if os.path.exists(f"{CSV_PATH}.backup"):
            os.rename(f"{CSV_PATH}.backup", CSV_PATH)
            print("Backup restored.")
        
        sys.exit(1)
    
    # Save
    df = format_df(df)
    df.to_csv(CSV_PATH, index=False)
    
    # Show what dates we have
    dates = pd.to_datetime(df['GAME_DATE'])
    
    print("\n" + "=" * 60)
    print("SUCCESS!")
    print(f"Saved {len(df)} records to {CSV_PATH}")
    print(f"Date range: {dates.min().strftime('%Y-%m-%d')} to {dates.max().strftime('%Y-%m-%d')}")
    print(f"Unique game dates: {df['GAME_DATE'].nunique()}")
    print(f"Finished at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("=" * 60)

if __name__ == "__main__":
    main()

# # 2025-26 games (year is when the season starts here)
# year = '2025'
# import sys
# import platform
# def py_cat(msg):
#     print(msg, file=sys.stderr)
# 
# os_name = platform.system()
# 
# # Skip if the file was already updated today (applies to both Darwin and Linux)
# import os
# from datetime import datetime, date
# 
# csv_path = "current_year.csv"
# skip = False
# if os.path.exists(csv_path):
#     mod_date = date.fromtimestamp(os.path.getmtime(csv_path))
#     if mod_date == date.today():
#         py_cat(f"{csv_path} was already updated today ({mod_date}). Skipping.")
#         skip = True
# 
# if not skip:
#     if os_name == "Darwin":
#         from nba_api.stats.endpoints import leaguegamefinder
#         gamefinder = leaguegamefinder.LeagueGameFinder()
#         games = gamefinder.get_data_frames()[0]
#         # Subset the games to when the last 4 digits of SEASON_ID were current year start.
#         season_games = games[games.SEASON_ID.str[-4:] == year]
#         season_games.to_csv("current_year.csv")
#     elif os_name == "Linux":
#         import time
#         # Add headers to look more like a browser
#         headers = {
#             'Host': 'stats.nba.com',
#             'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
#             'Accept': 'application/json, text/plain, */*',
#             'Accept-Language': 'en-US,en;q=0.9',
#             'Accept-Encoding': 'gzip, deflate, br',
#             'Connection': 'keep-alive',
#             'Referer': 'https://www.nba.com/',
#             'Origin': 'https://www.nba.com'
#         }
#         api_success = False
#         max_retries = 2
#         for attempt in range(max_retries):
#             try:
#                 py_cat(f"Attempt {attempt + 1}: Fetching data from NBA API...")
#                 from nba_api.stats.endpoints import leaguegamefinder
#                 gamefinder = leaguegamefinder.LeagueGameFinder(
#                     headers=headers,
#                     timeout=120
#                 )
#                 games = gamefinder.get_data_frames()[0]
#                 # Subset the games to when the last 4 digits of SEASON_ID were current year start
#                 season_games = games[games.SEASON_ID.str[-4:] == year]
#                 season_games.to_csv(csv_path, index=False)
#                 py_cat(f"Success! Updated {csv_path} with {len(season_games)} games.")
#                 api_success = True
#                 break
#             except Exception as e:
#                 py_cat(f"Attempt {attempt + 1} failed: {e}")
#                 if attempt < max_retries - 1:
#                     wait_time = 5 * (attempt + 1)
#                     py_cat(f"Waiting {wait_time} seconds before retry...")
#                     time.sleep(wait_time)
#         if not api_success:
#             if os.path.exists(csv_path):
#                 mod_time = datetime.fromtimestamp(os.path.getmtime(csv_path))
#                 py_cat(f"API failed. Using cached {csv_path} from {mod_time.strftime('%Y-%m-%d %H:%M')}")
#             else:
#                 raise Exception(f"API failed and no cached {csv_path} exists!")
