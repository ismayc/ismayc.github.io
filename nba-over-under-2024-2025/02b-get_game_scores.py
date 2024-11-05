# 2024-25 games

from nba_api.stats.endpoints import leaguegamefinder
year = '2024'
gamefinder = leaguegamefinder.LeagueGameFinder()
games = gamefinder.get_data_frames()[0]

# Subset the games to when the last 4 digits of SEASON_ID were current year start.
games_22 = games[games.SEASON_ID.str[-4:] == year]
games_22.to_csv("current_year.csv")
