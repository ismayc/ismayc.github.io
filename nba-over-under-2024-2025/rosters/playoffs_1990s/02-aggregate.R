library(tidyverse)

team_info <- read_csv("team_info.csv")

# Load the data
playoff_starters_cleaned <- read_rds("playoff_starters_all_enriched.rds") |> 
  mutate(team_full_name = str_replace_all(team_full_name, 
                                          " Basic and Advanced Stats Table",
                                          "")) |> 
  mutate(team_abbr = str_replace_all(team_abbr, 
                                     "BRK|NJN", "BKN")) |> 
  mutate(team_abbr = str_replace_all(team_abbr, "CHO|CHH", "CHA")) |> 
  left_join(team_info, by = c("team_abbr" = "abbreviation")) |> 
  mutate(
    # Remove the site name at the end
    game_info_clean = str_remove(game_info, " \\| Basketball-Reference\\.com$"),
    # Extract the series using regex
    series = str_extract(game_info_clean, "(?<=\\d{4} ).*(?= Game \\d+)"),
    # Extract the game number
    game = str_extract(game_info_clean, "(?<=Game )\\d+"),
    # Extract the matchup
    matchup = str_extract(game_info_clean, "(?<=: ).*(?=, \\w+ \\d{1,2}, \\d{4})"),
    # Extract the date string
    date_str = str_extract(game_info_clean, "\\w+ \\d{1,2}, \\d{4}$"),
    # Convert date string to date format
    game_date = as.Date(date_str, format = "%B %d, %Y")
  ) |>
  # Remove the intermediate columns if desired
  select(-game_info_clean, -date_str) |> 
  mutate(team_full_name = str_replace_all(team_full_name, 
                                          " Basic and Advanced Stats Table",
                                          ""))

east_even <- playoff_starters_cleaned |> 
  filter(conference == "East", season %% 2 == 0, season >= 1996)

# Count the number of playoff starts per player per team by season
playoff_starts_raw <- east_even |>
  group_by(season, team_abbr, player_name) |>
  summarize(playoff_starts = n(), .groups = "drop") |>
  arrange(season, team_abbr, desc(playoff_starts))

# Calculate the total number of playoff games each team played
total_playoff_games <- east_even |> 
  group_by(season, team_abbr) |> 
  summarize(total_playoff_games = n_distinct(game_link), .groups = "drop")

total_playoff_games |> 
  filter(
    team_abbr %in% c("TOR", "DET", "MIL"),
    season %in% c(2000, 2002, 2006)
    ) |> View()

# Merge the playoff starts with total playoff games and calculate relative frequency
playoff_starts <- playoff_starts_raw %>%
  left_join(total_playoff_games, by = c("season", "team_abbr")) %>%
  mutate(relative_percentage = round(playoff_starts / total_playoff_games * 100, 2)) |> 
  arrange(team_abbr, season)

top5_per_team <- playoff_starts |>
  group_by(season, team_abbr) |>
  top_n(5, playoff_starts) |> 
  ungroup()

top5_per_team |> 
  filter(
    team_abbr %in% c("DET", "MIL"),
    season %in% c(2000, 2006)
  ) |> View()

# Playoff appearances for each team
team_playoff_appearances <- east_even |> 
  distinct(season, team_abbr) 

num_team_playoff_appearances <- team_playoff_appearances |> 
  count(team_abbr)

# Bobcats playoff appearances
bobcats_playoff_appearances <- east_even |> 
  filter(str_detect(team_abbr, "CHA"))

nets_playoff_appearances <- east_even |> 
  filter(str_detect(team_full_name, "Nets"))

bulls_playoff_appearances <- east_even |> 
  filter(str_detect(team_full_name, "Bulls"))


# Determine the number of games played for by each team in each playoff year
total_team_playoff_games <- east_even |> 
  group_by(season, team_abbr) |> 
  summarize(total_playoff_games = n_distinct(game_link), .groups = "drop")


# Function to select unique teams and seasons with fixed assignment
select_unique_teams_and_seasons_fixed <- function(data, fixed_assignment) {
  # Get unique seasons and sort them
  all_seasons <- sort(unique(data$season))
  
  # Initialize variables to store the selection
  selected_teams <- vector("character", length(all_seasons))
  names(selected_teams) <- as.character(all_seasons)
  
  # Initialize a vector to track used teams
  used_teams <- character()
  
  # Apply the fixed assignment
  seasons <- all_seasons
  for (season in names(fixed_assignment)) {
    team <- fixed_assignment[[season]]
    selected_teams[season] <- team
    used_teams <- c(used_teams, team)
    # Remove the fixed season from the seasons to process
    seasons <- seasons[seasons != as.numeric(season)]
  }
  
  # Recursive function to perform backtracking
  select_team <- function(season_index) {
    if (season_index > length(seasons)) {
      return(TRUE)  # All seasons have been successfully assigned
    }
    
    season <- seasons[season_index]
    # Get teams that played in this season and are not yet used
    teams_in_season <- unique(data$team_abbr[data$season == season])
    available_teams <- setdiff(teams_in_season, used_teams)
    
    if (length(available_teams) == 0) {
      return(FALSE)  # No available team for this season, need to backtrack
    }
    
    for (team in available_teams) {
      # Choose the team
      selected_teams[as.character(season)] <<- team
      used_teams <<- c(used_teams, team)
      
      # Proceed to the next season
      if (select_team(season_index + 1)) {
        return(TRUE)
      }
      
      # Backtrack
      used_teams <<- setdiff(used_teams, team)
      selected_teams[as.character(season)] <<- NA
    }
    
    return(FALSE)
  }
  
  # Start the selection process
  success <- select_team(1)
  
  if (!success) {
    stop("Unable to select 15 unique teams and seasons with the given constraints.")
  }
  
  # Prepare the result data frame
  result <- data.frame(
    season = as.numeric(names(selected_teams)),
    team_abbr = selected_teams,
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Fixed assignment:
fixed_assignment <- list(
  '1996' = 'MIA', '1998' = 'CLE', #almost too hard
  '2000' = 'TOR', 
  '2002' = 'DET', '2004' = 'NYK',
  '2006' = 'MIL',
  '2008' = 'WAS', '2010' = 'CHA', 
  '2012' = 'PHI', '2014' = 'ATL',
  '2016' = 'BOS', '2018' = 'IND', '2020' = 'BKN',
  '2022' = 'CHI', '2024' = 'ORL'
)

# Apply the function
result <- select_unique_teams_and_seasons_fixed(team_playoff_appearances, fixed_assignment)

# View the result
print(result)

write_rds(result, "unique_teams_and_seasons_fixed2.rds")

## Picks for Phil

# Convert fixed_assignment to a data frame
fixed_assignment_df <- tibble(
  season = as.numeric(names(fixed_assignment)),
  team_abbr = unlist(fixed_assignment)
)

# Filter the total_playoff_games data frame
filtered_data <- total_playoff_games %>%
  semi_join(fixed_assignment_df, by = c("season", "team_abbr"))

# Return the starters from these teams
filtered_starters <- top5_per_team %>%
  semi_join(filtered_data, by = c("season", "team_abbr"))

picks_for_phil <- filtered_starters %>%
  distinct(season, team_abbr)

## Studying
west_odd <- playoff_starters_cleaned |> 
  filter(conference == "West", season %% 2 == 1, season >= 1995)

# Count the number of playoff starts per player per team by season
playoff_starts_west_raw <- west_odd |>
  group_by(season, team_abbr, player_name) |>
  summarize(playoff_starts = n(), .groups = "drop") |>
  arrange(season, team_abbr, desc(playoff_starts))

# Calculate the total number of playoff games each team played
total_west_playoff_games <- west_odd |> 
  group_by(season, team_abbr) |> 
  summarize(total_playoff_games = n_distinct(game_link), .groups = "drop")

# Merge the playoff starts with total playoff games and calculate relative frequency
playoff_west_starts <- playoff_starts_west_raw %>%
  left_join(total_west_playoff_games, by = c("season", "team_abbr")) %>%
  mutate(relative_percentage = round(playoff_starts / total_playoff_games * 100, 2)) |> 
  arrange(team_abbr, season)

top5_per_team_west <- playoff_west_starts |>
  group_by(season, team_abbr) |>
  top_n(5, playoff_starts) |> 
  arrange(season) |> 
  ungroup()

current_west <- top5_per_team_west |> 
  filter(!(season %in% c(2017, 2007, 1999, 2015, 2009, 1997, 2013, 2019, 2023, 2003, 2021, 2005, 2011))) |> 
  filter(!(team_abbr %in% c("OKC", "HOU", "SAC", "NOP", "SAS", "MIN", "GSW",
                            "LAC", "PHO", "UTA", "LAL", "MEM", "POR")))


# Summarize total playoff_starts per team per season
team_playoff_summary <- current_west %>%
  group_by(season, team_abbr) %>%                     # Group by season and team_abbr
  summarize(total_playoff_starts = sum(playoff_starts), .groups = 'drop')  # Sum playoff_starts

# Rank teams within each season based on total_playoff_starts
ranked_teams <- team_playoff_summary %>%
  arrange(season, desc(total_playoff_starts)) %>%    # Arrange by season and descending playoff starts
  group_by(season) %>%                                # Group by season for ranking
  mutate(rank = dense_rank(desc(total_playoff_starts))) %>%  # Assign ranks
  ungroup()                                           # Ungroup the data

# likely_picks <- tibble(
#   season = c(1995, 2001, 2005, 2011),
#   team_abbr = c("DEN", "POR", "MEM", "DAL")
# )
# 
# # Focus on the likely picks
# focused_data <- current_west |> 
#   semi_join(likely_picks, by = c("season", "team_abbr"))
