# 2024-25 games

year = '2024'

from nba_api.stats.endpoints import leaguegamefinder
gamefinder = leaguegamefinder.LeagueGameFinder()
games = gamefinder.get_data_frames()[0]

# Subset the games to when the last 4 digits of SEASON_ID were current year start.
season_games = games[games.SEASON_ID.str[-4:] == year]
season_games.to_csv("current_year.csv")
