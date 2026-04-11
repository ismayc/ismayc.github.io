library(tidyverse)
library(plotly)
library(forcats)

# =============================================================================
# NBA Over/Under Player Tendency Analysis -- ALL 6 SEASONS
# 2020-21 through 2025-26 (1,470 picks total)
# 2022-23 had 9 players (Jenelle's last + Andy's first overlap)
# =============================================================================

picks <- read_csv("all_picks_6seasons.csv") %>%
  mutate(choice = str_to_upper(choice))

# --- Outcomes ---
outcomes <- tribble(
  ~season, ~team, ~wins, ~projection,
  "2020-21","Atlanta Hawks",41,36.5,"2020-21","Boston Celtics",36,46.5,
  "2020-21","Brooklyn Nets",48,45.5,"2020-21","Charlotte Hornets",33,25.5,
  "2020-21","Chicago Bulls",31,29.5,"2020-21","Cleveland Cavaliers",22,22.5,
  "2020-21","Detroit Pistons",20,23.5,"2020-21","Indiana Pacers",34,39.5,
  "2020-21","Miami Heat",40,44.5,"2020-21","Milwaukee Bucks",46,50.5,
  "2020-21","New York Knicks",41,22.5,"2020-21","Orlando Magic",21,32.5,
  "2020-21","Philadelphia 76ers",49,44.5,"2020-21","Toronto Raptors",27,41.5,
  "2020-21","Washington Wizards",34,29.5,"2020-21","Dallas Mavericks",42,42.5,
  "2020-21","Denver Nuggets",47,44.5,"2020-21","Golden State Warriors",39,36.5,
  "2020-21","Houston Rockets",17,35.5,"2020-21","Los Angeles Clippers",47,46.5,
  "2020-21","Los Angeles Lakers",42,47.5,"2020-21","Memphis Grizzlies",38,31.5,
  "2020-21","Minnesota Timberwolves",23,27.5,"2020-21","New Orleans Pelicans",31,36.5,
  "2020-21","Oklahoma City Thunder",22,23.5,"2020-21","Phoenix Suns",51,38.5,
  "2020-21","Portland Trail Blazers",42,40.5,"2020-21","Sacramento Kings",31,28.5,
  "2020-21","San Antonio Spurs",33,28.5,"2020-21","Utah Jazz",52,40.5,
  "2021-22","Atlanta Hawks",43,46.5,"2021-22","Boston Celtics",51,45.5,
  "2021-22","Brooklyn Nets",44,57.5,"2021-22","Charlotte Hornets",43,39.5,
  "2021-22","Chicago Bulls",46,40.5,"2021-22","Cleveland Cavaliers",44,26.5,
  "2021-22","Detroit Pistons",23,25.5,"2021-22","Indiana Pacers",25,40.5,
  "2021-22","Miami Heat",53,48.5,"2021-22","Milwaukee Bucks",51,56.5,
  "2021-22","New York Knicks",37,43.5,"2021-22","Orlando Magic",22,22.5,
  "2021-22","Philadelphia 76ers",51,50.5,"2021-22","Toronto Raptors",48,35.5,
  "2021-22","Washington Wizards",35,35.5,"2021-22","Dallas Mavericks",52,48.5,
  "2021-22","Denver Nuggets",48,49.5,"2021-22","Golden State Warriors",53,48.5,
  "2021-22","Houston Rockets",20,22.5,"2021-22","Los Angeles Clippers",42,44.5,
  "2021-22","Los Angeles Lakers",33,52.5,"2021-22","Memphis Grizzlies",56,41.5,
  "2021-22","Minnesota Timberwolves",46,31.5,"2021-22","New Orleans Pelicans",36,37.5,
  "2021-22","Oklahoma City Thunder",24,22.5,"2021-22","Phoenix Suns",64,51.5,
  "2021-22","Portland Trail Blazers",27,43.5,"2021-22","Sacramento Kings",30,35.5,
  "2021-22","San Antonio Spurs",34,33.5,"2021-22","Utah Jazz",49,54.5,
  "2022-23","Atlanta Hawks",41,45.5,"2022-23","Boston Celtics",57,54.5,
  "2022-23","Brooklyn Nets",45,50.5,"2022-23","Charlotte Hornets",27,36.5,
  "2022-23","Chicago Bulls",40,43.5,"2022-23","Cleveland Cavaliers",51,46.5,
  "2022-23","Detroit Pistons",17,28.5,"2022-23","Indiana Pacers",35,24.5,
  "2022-23","Miami Heat",44,48.5,"2022-23","Milwaukee Bucks",58,52.5,
  "2022-23","New York Knicks",47,39.5,"2022-23","Orlando Magic",34,26.5,
  "2022-23","Philadelphia 76ers",54,50.5,"2022-23","Toronto Raptors",41,45.5,
  "2022-23","Washington Wizards",35,35.5,"2022-23","Dallas Mavericks",38,48.5,
  "2022-23","Denver Nuggets",53,49.5,"2022-23","Golden State Warriors",44,51.5,
  "2022-23","Houston Rockets",22,23.5,"2022-23","Los Angeles Clippers",44,52.5,
  "2022-23","Los Angeles Lakers",43,45.5,"2022-23","Memphis Grizzlies",51,48.5,
  "2022-23","Minnesota Timberwolves",42,48.5,"2022-23","New Orleans Pelicans",42,44.5,
  "2022-23","Oklahoma City Thunder",40,23.5,"2022-23","Phoenix Suns",45,51.5,
  "2022-23","Portland Trail Blazers",33,39.5,"2022-23","Sacramento Kings",48,33.5,
  "2022-23","San Antonio Spurs",22,22.5,"2022-23","Utah Jazz",37,25.5,
  "2023-24","Atlanta Hawks",36,41.5,"2023-24","Boston Celtics",64,54.5,
  "2023-24","Brooklyn Nets",32,38.5,"2023-24","Charlotte Hornets",21,30.5,
  "2023-24","Chicago Bulls",39,37.5,"2023-24","Cleveland Cavaliers",48,50.5,
  "2023-24","Detroit Pistons",14,26.5,"2023-24","Indiana Pacers",47,38.5,
  "2023-24","Miami Heat",46,46.5,"2023-24","Milwaukee Bucks",49,53.5,
  "2023-24","New York Knicks",50,45.5,"2023-24","Orlando Magic",47,37.5,
  "2023-24","Philadelphia 76ers",47,47.5,"2023-24","Toronto Raptors",25,36.5,
  "2023-24","Washington Wizards",15,24.5,"2023-24","Dallas Mavericks",50,43.5,
  "2023-24","Denver Nuggets",57,52.5,"2023-24","Golden State Warriors",46,47.5,
  "2023-24","Houston Rockets",41,29.5,"2023-24","Los Angeles Clippers",51,45.5,
  "2023-24","Los Angeles Lakers",47,46.5,"2023-24","Memphis Grizzlies",27,44.5,
  "2023-24","Minnesota Timberwolves",56,43.5,"2023-24","New Orleans Pelicans",49,42.5,
  "2023-24","Oklahoma City Thunder",57,44.5,"2023-24","Phoenix Suns",49,51.5,
  "2023-24","Portland Trail Blazers",21,23.5,"2023-24","Sacramento Kings",46,46.5,
  "2023-24","San Antonio Spurs",22,27.5,"2023-24","Utah Jazz",31,36.5,
  "2024-25","Atlanta Hawks",40,35.5,"2024-25","Boston Celtics",61,58.5,
  "2024-25","Brooklyn Nets",26,21.5,"2024-25","Charlotte Hornets",19,27.5,
  "2024-25","Chicago Bulls",39,30.5,"2024-25","Cleveland Cavaliers",64,47.5,
  "2024-25","Detroit Pistons",44,22.5,"2024-25","Indiana Pacers",50,46.5,
  "2024-25","Miami Heat",37,44.5,"2024-25","Milwaukee Bucks",48,51.5,
  "2024-25","New York Knicks",51,52.5,"2024-25","Orlando Magic",41,47.5,
  "2024-25","Philadelphia 76ers",24,52.5,"2024-25","Toronto Raptors",30,30.5,
  "2024-25","Washington Wizards",18,22.5,"2024-25","Dallas Mavericks",39,50.5,
  "2024-25","Denver Nuggets",50,52.5,"2024-25","Golden State Warriors",48,43.5,
  "2024-25","Houston Rockets",52,42.5,"2024-25","Los Angeles Clippers",50,42.5,
  "2024-25","Los Angeles Lakers",50,45.5,"2024-25","Memphis Grizzlies",48,45.5,
  "2024-25","Minnesota Timberwolves",49,52.5,"2024-25","New Orleans Pelicans",21,46.5,
  "2024-25","Oklahoma City Thunder",68,55.5,"2024-25","Phoenix Suns",36,46.5,
  "2024-25","Portland Trail Blazers",36,22.5,"2024-25","Sacramento Kings",40,46.5,
  "2024-25","San Antonio Spurs",34,35.5,"2024-25","Utah Jazz",17,28.5,
  "2025-26","Atlanta Hawks",45,46.5,"2025-26","Boston Celtics",54,40.5,
  "2025-26","Brooklyn Nets",20,20.5,"2025-26","Charlotte Hornets",43,28.5,
  "2025-26","Chicago Bulls",30,31.5,"2025-26","Cleveland Cavaliers",51,56.5,
  "2025-26","Detroit Pistons",58,45.5,"2025-26","Indiana Pacers",18,38.5,
  "2025-26","Miami Heat",41,38.5,"2025-26","Milwaukee Bucks",31,44.5,
  "2025-26","New York Knicks",51,53.5,"2025-26","Orlando Magic",44,51.5,
  "2025-26","Philadelphia 76ers",43,42.5,"2025-26","Toronto Raptors",44,38.5,
  "2025-26","Washington Wizards",17,20.5,"2025-26","Dallas Mavericks",25,40.5,
  "2025-26","Denver Nuggets",52,53.5,"2025-26","Golden State Warriors",37,46.5,
  "2025-26","Houston Rockets",50,53.5,"2025-26","Los Angeles Clippers",41,47.5,
  "2025-26","Los Angeles Lakers",50,48.5,"2025-26","Memphis Grizzlies",25,40.5,
  "2025-26","Minnesota Timberwolves",47,49.5,"2025-26","New Orleans Pelicans",26,30.5,
  "2025-26","Oklahoma City Thunder",64,62.5,"2025-26","Phoenix Suns",44,31.5,
  "2025-26","Portland Trail Blazers",40,33.5,"2025-26","Sacramento Kings",21,34.5,
  "2025-26","San Antonio Spurs",61,44.5,"2025-26","Utah Jazz",21,18.5,
) %>%
  mutate(outcome = if_else(wins > projection, "OVER", "UNDER"))

# --- Join ---
df <- picks %>%
  inner_join(outcomes %>% select(season, team, outcome), by = c("season", "team")) %>%
  mutate(correct = choice == outcome,
         points = if_else(correct, wage, -wage))

# Player ordering by lifetime points
player_order <- df %>%
  group_by(player) %>%
  summarize(pts = sum(points), .groups = "drop") %>%
  arrange(desc(pts)) %>%
  pull(player)

# =============================================================================
# Plot 1: Season score heatmap (plotly)
# =============================================================================
season_scores <- df %>%
  group_by(player, season) %>%
  summarize(points = sum(points), .groups = "drop") %>%
  mutate(label = paste0(if_else(points >= 0, "+", ""), points))

p1 <- season_scores %>%
  mutate(player = factor(player, levels = rev(player_order))) %>%
  ggplot(aes(x = season, y = player, fill = points,
             text = paste0(player, " ", season, ": ", label))) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = label), size = 3, fontface = "bold", color = "white") +
  scale_fill_gradient2(low = "#A32D2D", mid = "#888780", high = "#0F6E56",
                       midpoint = 0, name = "Points") +
  labs(title = "Player scores by season (all 6 seasons)", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) + theme(panel.grid = element_blank())

ggplotly(p1, tooltip = "text") %>% plotly::layout(margin = list(t = 50))

# =============================================================================
# Plot 2: OVER vs UNDER accuracy
# =============================================================================
ou_accuracy <- df %>%
  group_by(player, choice) %>%
  summarize(accuracy = round(mean(correct) * 100, 1), .groups = "drop") %>%
  mutate(player = factor(player, levels = player_order))

p2 <- ggplot(ou_accuracy, aes(x = player, y = accuracy, fill = choice)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("OVER" = "#185FA5", "UNDER" = "#A32D2D"), name = NULL) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  labs(title = "OVER vs UNDER accuracy by player",
       subtitle = "Every player is more accurate on UNDER picks",
       x = NULL, y = "Accuracy %") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
print(p2)

# =============================================================================
# Plot 3: High-confidence (13+) performance
# =============================================================================
high_conf <- df %>%
  filter(wage >= 13) %>%
  group_by(player) %>%
  summarize(picks = n(), accuracy = round(mean(correct) * 100, 1),
            points = sum(points), .groups = "drop") %>%
  mutate(player = factor(player, levels = player_order))

p3 <- ggplot(high_conf, aes(x = player, y = points, fill = points > 0)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(accuracy, "%"),
                y = if_else(points >= 0, points + 10, points - 10)), size = 3.2) +
  scale_fill_manual(values = c("TRUE" = "#0F6E56", "FALSE" = "#A32D2D"), guide = "none") +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  labs(title = "High-confidence picks (wager 13+): points and accuracy",
       subtitle = "Labels show accuracy %. Only Chester, Jenelle, and Mike are profitable.",
       x = NULL, y = "Points from 13+ wagers") +
  theme_minimal(base_size = 12)
print(p3)

# =============================================================================
# Plot 4: Contrarian vs consensus
# =============================================================================
team_majority <- df %>%
  count(season, team, choice) %>%
  group_by(season, team) %>%
  slice_max(n, n = 1) %>%
  ungroup() %>%
  select(season, team, majority = choice)

df_c <- df %>%
  inner_join(team_majority, by = c("season", "team")) %>%
  mutate(type = if_else(choice != majority, "Contrarian", "Consensus"))

contrarian_summary <- df_c %>%
  group_by(player, type) %>%
  summarize(picks = n(), accuracy = round(mean(correct)*100, 0),
            points = sum(points), .groups = "drop") %>%
  mutate(player = factor(player, levels = player_order))

p4 <- ggplot(contrarian_summary, aes(x = player, y = points, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_fill_manual(values = c("Consensus" = "#185FA5", "Contrarian" = "#BA7517"), name = NULL) +
  labs(title = "Points from contrarian vs consensus picks",
       x = NULL, y = "Total points") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
print(p4)

# =============================================================================
# Plot 5: OVER bias vs accuracy scatter
# =============================================================================
player_summary <- df %>%
  group_by(player) %>%
  summarize(accuracy = round(mean(correct) * 100, 1),
            pct_over = round(mean(choice == "OVER") * 100, 0),
            total_points = sum(points), .groups = "drop")

p5 <- ggplot(player_summary,
             aes(x = pct_over, y = accuracy, size = abs(total_points),
                 color = total_points > 0)) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "gray70") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray70") +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = player), nudge_y = 1.2, size = 3.5, color = "gray30") +
  scale_color_manual(values = c("TRUE" = "#0F6E56", "FALSE" = "#A32D2D"),
                     name = "Lifetime profitable?") +
  scale_size_continuous(range = c(4, 14), name = "|Total points|") +
  labs(title = "OVER bias vs accuracy (6 seasons)",
       subtitle = "Players who lean UNDER tend to be more accurate",
       x = "% of picks that are OVER", y = "Overall accuracy %") +
  theme_minimal(base_size = 12)
print(p5)

# =============================================================================
# Plot 6: Cumulative points progression (interactive plotly)
# =============================================================================
cumulative <- df %>%
  arrange(player, season) %>%
  group_by(player) %>%
  mutate(pick_num = row_number(),
         cum_points = cumsum(points),
         cum_accuracy = round(cumsum(correct) / pick_num * 100, 1)) %>%
  ungroup() %>%
  mutate(player = factor(player, levels = player_order))

p6 <- cumulative %>%
  ggplot(aes(x = pick_num, y = cum_points, color = player,
             text = paste0(player, " pick #", pick_num,
                           "\nSeason: ", season, "\nTeam: ", team,
                           "\nPick: ", choice, " (", wage, "pts)",
                           "\nOutcome: ", outcome,
                           "\nCumulative: ", cum_points))) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "gray70") +
  scale_color_brewer(palette = "Set2", name = NULL) +
  labs(title = "Cumulative point progression (all 6 seasons, hover for details)",
       x = "Pick number (chronological)", y = "Cumulative points") +
  theme_minimal(base_size = 12)

ggplotly(p6, tooltip = "text") %>%
  plotly::layout(legend = list(orientation = "h", y = -0.15),
                 margin = list(t = 60, b = 80))

# =============================================================================
# Plot 7: Average wage by pick direction
# =============================================================================
wage_by_choice <- df %>%
  group_by(player, choice) %>%
  summarize(avg_wage = round(mean(wage), 1), .groups = "drop") %>%
  mutate(player = factor(player, levels = player_order))

p7 <- ggplot(wage_by_choice, aes(x = player, y = avg_wage, fill = choice)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("OVER" = "#185FA5", "UNDER" = "#A32D2D"), name = NULL) +
  labs(title = "Average wager by pick direction",
       subtitle = "Most players put more points on OVERs (the less accurate direction)",
       x = NULL, y = "Average wager") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
print(p7)

# =============================================================================
# Summary
# =============================================================================
cat("\n========== FULL 6-SEASON SUMMARY (1,470 picks) ==========\n\n")
df %>%
  group_by(player) %>%
  summarize(
    seasons = n_distinct(season), picks = n(),
    correct = sum(correct), accuracy = round(mean(correct)*100,1),
    total_pts = sum(points),
    over_pct = round(mean(choice=="OVER")*100,0),
    over_acc = round(mean(correct[choice=="OVER"])*100,1),
    under_acc = round(mean(correct[choice=="UNDER"])*100,1),
    .groups = "drop"
  ) %>%
  arrange(desc(total_pts)) %>%
  print(n = 20)
