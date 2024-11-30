import requests
from bs4 import BeautifulSoup
import pandas as pd

# Example for one season
season = 1991
playoffs_url = f'https://www.basketball-reference.com/playoffs/NBA_{season}.html'
response = requests.get(playoffs_url)
soup = BeautifulSoup(response.content, 'html.parser')

# Find all playoff series
series_links = [a['href'] for a in soup.select('a[href*="NBA_"][href*="_games.html"]')]

# Loop through series and games
for series_link in series_links:
    series_url = 'https://www.basketball-reference.com' + series_link
    series_response = requests.get(series_url)
    series_soup = BeautifulSoup(series_response.content, 'html.parser')
    game_links = [a['href'] for a in series_soup.select('a[href*="boxscores"]')]

    for game_link in game_links:
        game_url = 'https://www.basketball-reference.com' + game_link
        game_response = requests.get(game_url)
        game_soup = BeautifulSoup(game_response.content, 'html.parser')

        # Extract starting lineups
        # (You will need to inspect the page structure to write the correct selectors)

