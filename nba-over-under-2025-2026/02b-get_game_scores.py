# 2025-26 games (year is when the season starts here)
year = '2025'
import platform
os_name = platform.system()

if os_name == "Darwin":
    from nba_api.stats.endpoints import leaguegamefinder
    gamefinder = leaguegamefinder.LeagueGameFinder()
    games = gamefinder.get_data_frames()[0]

    # Subset the games to when the last 4 digits of SEASON_ID were current year start.
    season_games = games[games.SEASON_ID.str[-4:] == year]
    season_games.to_csv("current_year.csv")

elif os_name == "Linux":
    import os
    import time
    from datetime import datetime

    # Path to the CSV (adjust if needed)
    csv_path = "current_year.csv"

    # Add headers to look more like a browser
    headers = {
        'Host': 'stats.nba.com',
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
        'Accept': 'application/json, text/plain, */*',
        'Accept-Language': 'en-US,en;q=0.9',
        'Accept-Encoding': 'gzip, deflate, br',
        'Connection': 'keep-alive',
        'Referer': 'https://www.nba.com/',
        'Origin': 'https://www.nba.com'
    }

    api_success = False
    max_retries = 3

    for attempt in range(max_retries):
        try:
            cat(f"Attempt {attempt + 1}: Fetching data from NBA API...")
            from nba_api.stats.endpoints import leaguegamefinder

            gamefinder = leaguegamefinder.LeagueGameFinder(
                headers=headers,
                timeout=120
            )
            games = gamefinder.get_data_frames()[0]

            # Subset the games to when the last 4 digits of SEASON_ID were current year start
            season_games = games[games.SEASON_ID.str[-4:] == year]
            season_games.to_csv(csv_path, index=False)

            cat(f"Success! Updated {csv_path} with {len(season_games)} games.")
            api_success = True
            break

        except Exception as e:
            cat(f"Attempt {attempt + 1} failed: {e}")
            if attempt < max_retries - 1:
                wait_time = 30 * (attempt + 1)
                cat(f"Waiting {wait_time} seconds before retry...")
                time.sleep(wait_time)

    if not api_success:
        if os.path.exists(csv_path):
            mod_time = datetime.fromtimestamp(os.path.getmtime(csv_path))
            cat(f"API failed. Using cached {csv_path} from {mod_time.strftime('%Y-%m-%d %H:%M')}")
        else:
            raise Exception(f"API failed and no cached {csv_path} exists!")
