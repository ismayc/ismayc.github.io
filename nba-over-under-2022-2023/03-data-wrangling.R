library(tidyverse)
library(plotly)
library(here)
library(glue)
#library(zoo)

standings <- here("rds", glue("standings_through_{Sys.Date() - 1}.rds")) %>% 
  read_rds()

game_results_raw <- here(
  "rds", 
  glue("game_results_raw_through_{Sys.Date() - 1}.rds")) %>% 
  read_rds()

game_results <- game_results_raw %>% 
  mutate(win = if_else(slugTeam != slugTeamLoser, 1, 0),
         loss = 1 - win) %>% 
  group_by(nameTeam) %>% 
  #  mutate(current_win_total = rollapplyr(win, 2, sum, partial = TRUE),
  #         current_loss_total = rollapplyr(loss, 2, sum, partial = TRUE)) %>% 
  mutate(
    current_wins = cumsum(win),
    current_losses = cumsum(loss),
    current_win_perc = current_wins / (current_wins + current_losses) * 100
  ) %>% 
  rename(`Game Number` = numberGameTeamSeason,
         `Team Name` = nameTeam,
         `Game Date` = dateGame) %>% 
  mutate(`Team Name` = str_replace_all(`Team Name`, "LA ", "Los Angeles ")) %>% 
  arrange(`Team Name`)

scores_tidy <- scores %>% 
  pivot_longer(cols = away_score:home_score,
               names_to = "location",
               values_to = "score") %>% 
  mutate(location = str_replace_all(location, "_score", "")) %>% 
  mutate(
    team_slug = if_else(location == "away", slug_away_team, slug_home_team),
    opponent_slug = if_else(location == "away", slug_home_team, slug_away_team),
    team_name = if_else(location == "away", away_team, home_team)
  ) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) %>% 
  mutate(opponent_score = if_else(rowname %% 2 == 1,
                                  lead(score),
                                  lag(score)))

updated_schedule <- game_results %>% 
  inner_join(
    scores_tidy %>% 
      select(game_id, game_date, team_name, score, opponent_score), 
    by = c("Game Date" = "game_date", "Team Name" = "team_name")
  ) %>% 
  relocate(game_id, .after = `Game Date`) %>% 
  mutate(final_score = paste0(score, "-", opponent_score)) %>% 
  mutate(`Current Record` = paste0(current_wins, "-", current_losses)) %>% 
  mutate(`Game Result` = paste0("\nGame Number: ", `Game Number`, "\n",
                                "Game Date: ", `Game Date`, "\n",
                                "Game Matchup: ", slugMatchup, "\n", 
                                "Final Score: ", final_score))

join_with_projections <- updated_schedule %>% 
  inner_join(projections, by = c("Team Name" = "team")) %>% 
  mutate(`Current Win %` = round(current_win_perc, 2),
         `Vegas Insider Win Projection %` = round(percentage_projection, 2)) %>% 
  mutate(over_under = case_when(
    `Current Win %` > `Vegas Insider Win Projection %` ~ "OVER",
    `Current Win %` < `Vegas Insider Win Projection %` ~ "UNDER",
    TRUE                                               ~ "PUSH")
  ) %>% 
  group_by(`Team Name`) %>% 
  mutate(flipped = if_else(lag(over_under) != over_under, TRUE, NA),
         flipped_today = if_else(as.Date(`Game Date`) == Sys.Date() - 1,
                                 TRUE,
                                 NA))

# Pulling in picks
days_in_season_to_today <- seq(from = as.Date(nba_season_start_date), 
                               to = as.Date(Sys.Date() - 1), 
                               by = "days")
teams <- unique(join_with_projections$`Team Name`)

standings_grid <- crossing(team = teams, date = days_in_season_to_today) %>% 
  left_join(join_with_projections %>% 
              select(
                `Team Name`, `Game Date`, starts_with("current"),
                `Vegas Insider Win Projection %`, over_under,
                -current_win_perc
              ), 
            by = c("team" = "Team Name", "date" = "Game Date")) %>% 
  group_by(team) %>% 
  fill(c(current_wins, current_losses, `Current Record`, `Current Win %`, 
         over_under), 
       .direction = "down") %>% 
  fill(`Vegas Insider Win Projection %`, .direction = "downup") %>% 
  replace_na(replace = list(current_wins = 0,
                            current_losses = 0,
                            `Current Record` = "0-0",
                            `Current Win %` = NA_real_))

with_picks <- standings_grid %>% 
  inner_join(picks, by = "team") %>% 
  mutate(current_projected_points = if_else(
    over_under == choice,
    wage,
    -wage
  ))

player_projections_by_team <- with_picks %>% 
  select(Date = date, Player = player, Team = team, starts_with("current"),
         `Vegas Insider Win Projection %`,
         `Current Over/Under` = over_under,
         `Over/Under Pick` = choice) %>% 
  mutate(
    `Win % - Vegas Insider` = round(`Current Win %` - `Vegas Insider Win Projection %`, 2)
  ) %>% 
  relocate(current_projected_points, 
           .after = `Win % - Vegas Insider`)

changes_in_player_projections <- player_projections_by_team %>% 
  group_by(Player, Team) %>% 
  mutate(
    `Change in Points Since Yesterday` = current_projected_points - lag(current_projected_points)
  ) %>% 
  ungroup() %>% 
  arrange(Date, Team, Player) %>% 
  filter(`Change in Points Since Yesterday` != 0) %>% 
  select(Date, Player, Team, `Current Record`, `Current Win %`, 
         `Vegas Insider Win Projection %`, `Change in Points Since Yesterday`)



rows_today <- changes_in_player_projections %>% 
  filter(Date == Sys.Date() - 1)

projected_score <- player_projections_by_team %>% 
  group_by(Date, Player) %>% 
  summarize(
    `Projected Total Points` = sum(current_projected_points, na.rm = TRUE),
    `Number Correct` = sum(current_projected_points > 0),
    `Number (Wage 15) Correct` = sum(
      current_projected_points == 15, na.rm = TRUE
    ),
    `Number (Wage 14) Correct` = sum(
      current_projected_points == 14, na.rm = TRUE
    ),
    `Number (Wage 13) Correct` = sum(
      current_projected_points == 13, na.rm = TRUE
    ),
    `Number (Wage 12) Correct` = sum(
      current_projected_points == 12, na.rm = TRUE
    ),
    .groups = "drop"
  )

player_projections_by_team <- player_projections_by_team %>% 
  rename(`Current Projected Points` = current_projected_points,
         `Current Wins` = current_wins,
         `Current Losses` = current_losses) %>% 
  mutate(
    `Wins To Go Over Vegas Insider` = 
      ceiling(`Vegas Insider Win Projection %` / 100 * num_games) - `Current Wins`,
    `Winning % In Remaining Games Needed` = round(
      `Wins To Go Over Vegas Insider` / 
        (num_games - `Current Wins` - `Current Losses`) * 100, 2),
    .before = `Current Projected Points`,
    `Losses To Go Under Vegas Insider` = 
      num_games - ceiling(`Vegas Insider Win Projection %` / 100 * num_games) - 
      `Current Losses`) %>% 
  select(-`Current Wins`, -`Current Losses`) %>% 
  mutate(`Outcome Determined` = factor(case_when(
    `Winning % In Remaining Games Needed` <= 0 ~ "OVER",
    `Winning % In Remaining Games Needed` > 100 ~ "UNDER",
    TRUE ~ "not yet"),
    levels = c("not yet", "OVER", "UNDER")
  ))  

most_recent_results <- projected_score %>% 
  mutate(days_from_today = as.numeric(difftime(Sys.Date(),
                                               Date,
                                               units = c("days")))) %>% 
  filter(days_from_today == min(days_from_today))

current_rankings <- most_recent_results %>% 
  arrange(desc(`Projected Total Points`),
          desc(`Number Correct`),
          desc(`Number (Wage 15) Correct`)) %>% 
  pull(Player)

# As a check to compare to Phil's report
# previous_reported_results <- projected_score %>% 
#   filter(Date == as.Date("2021-01-28")) %>% 
#   arrange(desc(`Projected Total Points`))
# 
# previous_projected_scores <- player_projections_by_team %>% 
#   filter(Date == as.Date("2021-01-28"))

standings_with_projected <- standings %>% 
  mutate(date = Sys.Date() - 1) %>% 
  mutate(wins = as.numeric(wins),
         losses = as.numeric(losses)) %>% 
  mutate(current_win_perc = wins / (wins + losses) * 100) %>% 
  inner_join(projections, by = c("team_name" = "team")) %>% 
  select(-percentage_projection) %>% 
  mutate(
    remaining_games = num_games - (wins + losses),
    wins_needed_for_over = ceiling(as.numeric(win_projection)) - wins,
    winning_perc_for_over = wins_needed_for_over / remaining_games * 100
  ) %>% 
  mutate(differential = current_win_perc - winning_perc_for_over)

remaining_check <- standings_with_projected %>% 
  inner_join(picks, by = c("team_name" = "team"))

chester_check <- remaining_check %>% 
  filter(player == "Chester") %>% 
  select(-player)
