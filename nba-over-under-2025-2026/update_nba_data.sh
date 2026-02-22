#!/bin/bash
set -e
export PATH="/usr/local/bin:/usr/bin:/bin:/opt/homebrew/bin:$PATH"
cd ~/Desktop/repos/ismayc.github.io/nba-over-under-2025-2026/
git remote set-url origin git@github.com:ismayc/ismayc.github.io.git

/Users/chesterismay/.virtualenvs/r-reticulate/bin/python3 << 'EOF'
from nba_api.stats.endpoints import leaguegamefinder
import time

year = '2025'
max_retries = 3
for attempt in range(max_retries):
    try:
        gamefinder = leaguegamefinder.LeagueGameFinder(timeout=120)
        games = gamefinder.get_data_frames()[0]
        season_games = games[games.SEASON_ID.str[-4:] == year]
        season_games.to_csv("current_year.csv", index=False)
        print(f"Updated current_year.csv with {len(season_games)} games")
        break
    except Exception as e:
        print(f"Attempt {attempt + 1} failed: {e}")
        if attempt < max_retries - 1:
            time.sleep(10)
        else:
            print("All retries failed")
            exit(1)
EOF

git config --local user.email "chester.ismay@gmail.com"
git config --local user.name "Chester Ismay"
git fetch origin master
git reset --soft origin/master
git add current_year.csv
git commit -m "Update NBA data $(date +%Y-%m-%d)"
git push origin master