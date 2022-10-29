# 2022-23 games

from nba_api.stats.endpoints import leaguegamefinder

gamefinder = leaguegamefinder.LeagueGameFinder()
games = gamefinder.get_data_frames()[0]

# Subset the games to when the last 4 digits of SEASON_ID were 2022.
games_22 = games[games.SEASON_ID.str[-4:] == '2022']
games_22.to_csv("current_year.csv")
