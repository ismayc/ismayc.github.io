#!/bin/bash
cd ~/Desktop/repos/ismayc.github.io/nba-over-under-2025-2026/

pip install nba_api

python3 << 'EOF'
year = '2025'
from nba_api.stats.endpoints import leaguegamefinder
gamefinder = leaguegamefinder.LeagueGameFinder()
games = gamefinder.get_data_frames()[0]
season_games = games[games.SEASON_ID.str[-4:] == year]
season_games.to_csv("current_year.csv", index=False)
print(f"Updated current_year.csv with {len(season_games)} games")
EOF

git config --local user.email "chester.ismay@gmail.com"
git config --local user.name "Chester Ismay"
git fetch origin master
git reset --soft origin/master
git add current_year.csv
git commit -m "Update NBA data $(date +%Y-%m-%d)"
git push origin master