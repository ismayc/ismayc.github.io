library(dplyr)
library(lubridate)

# 1. Prepare Winning Percentages
# Assuming 'standings' data frame has the winning percentages in the 'Winning Pct' column
winning_pct <- standings |>
  select(team_name, Winning_Pct = `Winning Pct`)

# 2. Filter Remaining Schedule
# Find the latest game date from 'scores_tidy'
latest_game_date <- max(scores_tidy$game_date)

# Filter the 'schedule' for games after the latest game date
remaining_schedule <- schedule |>
  filter(game_date > latest_game_date)

# 3. Calculate Remaining SOS for Each Team
# Combine the remaining schedule with the winning percentages of the opponents
remaining_schedule_with_wp <- remaining_schedule |>
  left_join(winning_pct, by = c("away_team" = "team_name")) |>
  rename(AwayTeamWP = Winning_Pct) |>
  left_join(winning_pct, by = c("home_team" = "team_name")) |>
  rename(HomeTeamWP = Winning_Pct)

# Calculate the average winning percentage for home and away games separately
home_sos <- remaining_schedule_with_wp |>
  group_by(home_team) |>
  summarise(Avg_Opponent_WP = mean(AwayTeamWP, na.rm = TRUE))

away_sos <- remaining_schedule_with_wp |>
  group_by(away_team) |>
  summarise(Avg_Opponent_WP = mean(HomeTeamWP, na.rm = TRUE))

# Combine the SOS for home and away to get the overall remaining SOS for each team
combined_sos <- bind_rows(home_sos, away_sos) |>
  group_by(team = coalesce(home_team, away_team)) |>
  summarise(`Remaining Opponent Winning Percentage` = mean(Avg_Opponent_WP, na.rm = TRUE) * 100)

# View the remaining strength of schedule
View(combined_sos)
