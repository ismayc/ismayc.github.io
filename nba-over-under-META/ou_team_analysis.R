library(tidyverse)
library(forcats)
library(tidytext)

# =============================================================================
# Five-year NBA Over/Under vs Playoff Status Analysis
# Seasons: 2020-21 through 2025-26
# =============================================================================

# All data compiled from Vegas projections (picks.xlsx) and final standings
# 2020-21 was a 72-game season; projections were already on a 72-game scale

results <- tribble(
  ~season,     ~team,                        ~wins, ~losses, ~projection, ~conference,
  # --- 2020-21 (72-game season, MIL won title) ---
  "2020-21", "Utah Jazz",                     52, 20, 40.5, "West",
  "2020-21", "Phoenix Suns",                  51, 21, 38.5, "West",
  "2020-21", "Los Angeles Clippers",          47, 25, 46.5, "West",
  "2020-21", "Denver Nuggets",                47, 25, 44.5, "West",
  "2020-21", "Dallas Mavericks",              42, 30, 42.5, "West",
  "2020-21", "Portland Trail Blazers",        42, 30, 40.5, "West",
  "2020-21", "Los Angeles Lakers",            42, 30, 47.5, "West",
  "2020-21", "Golden State Warriors",         39, 33, 36.5, "West",
  "2020-21", "Memphis Grizzlies",             38, 34, 31.5, "West",
  "2020-21", "San Antonio Spurs",             33, 39, 28.5, "West",
  "2020-21", "New Orleans Pelicans",          31, 41, 36.5, "West",
  "2020-21", "Sacramento Kings",              31, 41, 28.5, "West",
  "2020-21", "Minnesota Timberwolves",        23, 49, 27.5, "West",
  "2020-21", "Oklahoma City Thunder",         22, 50, 23.5, "West",
  "2020-21", "Houston Rockets",               17, 55, 35.5, "West",
  "2020-21", "Philadelphia 76ers",            49, 23, 44.5, "East",
  "2020-21", "Brooklyn Nets",                 48, 24, 45.5, "East",
  "2020-21", "Milwaukee Bucks",               46, 26, 50.5, "East",
  "2020-21", "Atlanta Hawks",                 41, 31, 36.5, "East",
  "2020-21", "New York Knicks",               41, 31, 22.5, "East",
  "2020-21", "Miami Heat",                    40, 32, 44.5, "East",
  "2020-21", "Boston Celtics",                36, 36, 46.5, "East",
  "2020-21", "Indiana Pacers",                34, 38, 39.5, "East",
  "2020-21", "Washington Wizards",            34, 38, 29.5, "East",
  "2020-21", "Charlotte Hornets",             33, 39, 25.5, "East",
  "2020-21", "Chicago Bulls",                 31, 41, 29.5, "East",
  "2020-21", "Toronto Raptors",               27, 45, 41.5, "East",
  "2020-21", "Cleveland Cavaliers",           22, 50, 22.5, "East",
  "2020-21", "Orlando Magic",                 21, 51, 32.5, "East",
  "2020-21", "Detroit Pistons",               20, 52, 23.5, "East",
  
  # --- 2021-22 (GSW won title) ---
  "2021-22", "Phoenix Suns",                  64, 18, 51.5, "West",
  "2021-22", "Memphis Grizzlies",             56, 26, 41.5, "West",
  "2021-22", "Golden State Warriors",         53, 29, 48.5, "West",
  "2021-22", "Dallas Mavericks",              52, 30, 48.5, "West",
  "2021-22", "Utah Jazz",                     49, 33, 54.5, "West",
  "2021-22", "Denver Nuggets",                48, 34, 49.5, "West",
  "2021-22", "Minnesota Timberwolves",        46, 36, 31.5, "West",
  "2021-22", "Los Angeles Clippers",          42, 40, 44.5, "West",
  "2021-22", "New Orleans Pelicans",          36, 46, 37.5, "West",
  "2021-22", "San Antonio Spurs",             34, 48, 33.5, "West",
  "2021-22", "Los Angeles Lakers",            33, 49, 52.5, "West",
  "2021-22", "Sacramento Kings",              30, 52, 35.5, "West",
  "2021-22", "Portland Trail Blazers",        27, 55, 43.5, "West",
  "2021-22", "Oklahoma City Thunder",         24, 58, 22.5, "West",
  "2021-22", "Houston Rockets",               20, 62, 22.5, "West",
  "2021-22", "Miami Heat",                    53, 29, 48.5, "East",
  "2021-22", "Boston Celtics",                51, 31, 45.5, "East",
  "2021-22", "Milwaukee Bucks",               51, 31, 56.5, "East",
  "2021-22", "Philadelphia 76ers",            51, 31, 50.5, "East",
  "2021-22", "Toronto Raptors",               48, 34, 35.5, "East",
  "2021-22", "Chicago Bulls",                 46, 36, 40.5, "East",
  "2021-22", "Cleveland Cavaliers",           44, 38, 26.5, "East",
  "2021-22", "Brooklyn Nets",                 44, 38, 57.5, "East",
  "2021-22", "Atlanta Hawks",                 43, 39, 46.5, "East",
  "2021-22", "Charlotte Hornets",             43, 39, 39.5, "East",
  "2021-22", "New York Knicks",               37, 45, 43.5, "East",
  "2021-22", "Washington Wizards",            35, 47, 35.5, "East",
  "2021-22", "Indiana Pacers",                25, 57, 40.5, "East",
  "2021-22", "Detroit Pistons",               23, 59, 25.5, "East",
  "2021-22", "Orlando Magic",                 22, 60, 22.5, "East",
  
  # --- 2022-23 (DEN won title) ---
  "2022-23", "Denver Nuggets",                53, 29, 49.5, "West",
  "2022-23", "Memphis Grizzlies",             51, 31, 48.5, "West",
  "2022-23", "Sacramento Kings",              48, 34, 33.5, "West",
  "2022-23", "Phoenix Suns",                  45, 37, 51.5, "West",
  "2022-23", "Los Angeles Clippers",          44, 38, 52.5, "West",
  "2022-23", "Golden State Warriors",         44, 38, 51.5, "West",
  "2022-23", "Los Angeles Lakers",            43, 39, 45.5, "West",
  "2022-23", "Minnesota Timberwolves",        42, 40, 48.5, "West",
  "2022-23", "New Orleans Pelicans",          42, 40, 44.5, "West",
  "2022-23", "Oklahoma City Thunder",         40, 42, 23.5, "West",
  "2022-23", "Dallas Mavericks",              38, 44, 48.5, "West",
  "2022-23", "Utah Jazz",                     37, 45, 25.5, "West",
  "2022-23", "Portland Trail Blazers",        33, 49, 39.5, "West",
  "2022-23", "Houston Rockets",               22, 60, 23.5, "West",
  "2022-23", "San Antonio Spurs",             22, 60, 22.5, "West",
  "2022-23", "Milwaukee Bucks",               58, 24, 52.5, "East",
  "2022-23", "Boston Celtics",                57, 25, 54.5, "East",
  "2022-23", "Philadelphia 76ers",            54, 28, 50.5, "East",
  "2022-23", "Cleveland Cavaliers",           51, 31, 46.5, "East",
  "2022-23", "New York Knicks",               47, 35, 39.5, "East",
  "2022-23", "Brooklyn Nets",                 45, 37, 50.5, "East",
  "2022-23", "Miami Heat",                    44, 38, 48.5, "East",
  "2022-23", "Atlanta Hawks",                 41, 41, 45.5, "East",
  "2022-23", "Toronto Raptors",               41, 41, 45.5, "East",
  "2022-23", "Chicago Bulls",                 40, 42, 43.5, "East",
  "2022-23", "Washington Wizards",            35, 47, 35.5, "East",
  "2022-23", "Indiana Pacers",                35, 47, 24.5, "East",
  "2022-23", "Orlando Magic",                 34, 48, 26.5, "East",
  "2022-23", "Charlotte Hornets",             27, 55, 36.5, "East",
  "2022-23", "Detroit Pistons",               17, 65, 28.5, "East",
  
  # --- 2023-24 (BOS won title) ---
  "2023-24", "Oklahoma City Thunder",         57, 25, 44.5, "West",
  "2023-24", "Denver Nuggets",                57, 25, 52.5, "West",
  "2023-24", "Minnesota Timberwolves",        56, 26, 43.5, "West",
  "2023-24", "Los Angeles Clippers",          51, 31, 45.5, "West",
  "2023-24", "Dallas Mavericks",              50, 32, 43.5, "West",
  "2023-24", "Phoenix Suns",                  49, 33, 51.5, "West",
  "2023-24", "New Orleans Pelicans",          49, 33, 42.5, "West",
  "2023-24", "Los Angeles Lakers",            47, 35, 46.5, "West",
  "2023-24", "Sacramento Kings",              46, 36, 46.5, "West",
  "2023-24", "Golden State Warriors",         46, 36, 47.5, "West",
  "2023-24", "Houston Rockets",               41, 41, 29.5, "West",
  "2023-24", "Utah Jazz",                     31, 51, 36.5, "West",
  "2023-24", "Memphis Grizzlies",             27, 55, 44.5, "West",
  "2023-24", "San Antonio Spurs",             22, 60, 27.5, "West",
  "2023-24", "Portland Trail Blazers",        21, 61, 23.5, "West",
  "2023-24", "Boston Celtics",                64, 18, 54.5, "East",
  "2023-24", "New York Knicks",               50, 32, 45.5, "East",
  "2023-24", "Milwaukee Bucks",               49, 33, 53.5, "East",
  "2023-24", "Cleveland Cavaliers",           48, 34, 50.5, "East",
  "2023-24", "Indiana Pacers",                47, 35, 38.5, "East",
  "2023-24", "Philadelphia 76ers",            47, 35, 47.5, "East",
  "2023-24", "Orlando Magic",                 47, 35, 37.5, "East",
  "2023-24", "Miami Heat",                    46, 36, 46.5, "East",
  "2023-24", "Chicago Bulls",                 39, 43, 37.5, "East",
  "2023-24", "Atlanta Hawks",                 36, 46, 41.5, "East",
  "2023-24", "Brooklyn Nets",                 32, 50, 38.5, "East",
  "2023-24", "Toronto Raptors",               25, 57, 36.5, "East",
  "2023-24", "Charlotte Hornets",             21, 61, 30.5, "East",
  "2023-24", "Washington Wizards",            15, 67, 24.5, "East",
  "2023-24", "Detroit Pistons",               14, 68, 26.5, "East",
  
  # --- 2024-25 (OKC won title) ---
  "2024-25", "Oklahoma City Thunder",         68, 14, 55.5, "West",
  "2024-25", "Houston Rockets",               52, 30, 42.5, "West",
  "2024-25", "Los Angeles Lakers",            50, 32, 45.5, "West",
  "2024-25", "Denver Nuggets",                50, 32, 52.5, "West",
  "2024-25", "Los Angeles Clippers",          50, 32, 42.5, "West",
  "2024-25", "Minnesota Timberwolves",        49, 33, 52.5, "West",
  "2024-25", "Golden State Warriors",         48, 34, 43.5, "West",
  "2024-25", "Memphis Grizzlies",             48, 34, 45.5, "West",
  "2024-25", "Sacramento Kings",              40, 42, 46.5, "West",
  "2024-25", "Dallas Mavericks",              39, 43, 50.5, "West",
  "2024-25", "Phoenix Suns",                  36, 46, 46.5, "West",
  "2024-25", "Portland Trail Blazers",        36, 46, 22.5, "West",
  "2024-25", "San Antonio Spurs",             34, 48, 35.5, "West",
  "2024-25", "New Orleans Pelicans",          21, 61, 46.5, "West",
  "2024-25", "Utah Jazz",                     17, 65, 28.5, "West",
  "2024-25", "Cleveland Cavaliers",           64, 18, 47.5, "East",
  "2024-25", "Boston Celtics",                61, 21, 58.5, "East",
  "2024-25", "New York Knicks",               51, 31, 52.5, "East",
  "2024-25", "Indiana Pacers",                50, 32, 46.5, "East",
  "2024-25", "Milwaukee Bucks",               48, 34, 51.5, "East",
  "2024-25", "Detroit Pistons",               44, 38, 22.5, "East",
  "2024-25", "Orlando Magic",                 41, 41, 47.5, "East",
  "2024-25", "Miami Heat",                    37, 45, 44.5, "East",
  "2024-25", "Atlanta Hawks",                 40, 42, 35.5, "East",
  "2024-25", "Chicago Bulls",                 39, 43, 30.5, "East",
  "2024-25", "Toronto Raptors",               30, 52, 30.5, "East",
  "2024-25", "Brooklyn Nets",                 26, 56, 21.5, "East",
  "2024-25", "Philadelphia 76ers",            24, 58, 52.5, "East",
  "2024-25", "Charlotte Hornets",             19, 63, 27.5, "East",
  "2024-25", "Washington Wizards",            18, 64, 22.5, "East",
  
  # --- 2025-26 (in progress, through Apr 8) ---
  "2025-26", "Oklahoma City Thunder",         64, 16, 62.5, "West",
  "2025-26", "San Antonio Spurs",             61, 19, 44.5, "West",
  "2025-26", "Denver Nuggets",                52, 28, 53.5, "West",
  "2025-26", "Los Angeles Lakers",            50, 29, 48.5, "West",
  "2025-26", "Houston Rockets",               50, 29, 53.5, "West",
  "2025-26", "Minnesota Timberwolves",        47, 33, 49.5, "West",
  "2025-26", "Phoenix Suns",                  44, 36, 31.5, "West",
  "2025-26", "Los Angeles Clippers",          41, 39, 47.5, "West",
  "2025-26", "Portland Trail Blazers",        40, 40, 33.5, "West",
  "2025-26", "Golden State Warriors",         37, 42, 46.5, "West",
  "2025-26", "New Orleans Pelicans",          26, 54, 30.5, "West",
  "2025-26", "Memphis Grizzlies",             25, 55, 40.5, "West",
  "2025-26", "Dallas Mavericks",              25, 55, 40.5, "West",
  "2025-26", "Sacramento Kings",              21, 59, 34.5, "West",
  "2025-26", "Utah Jazz",                     21, 59, 18.5, "West",
  "2025-26", "Detroit Pistons",               58, 22, 45.5, "East",
  "2025-26", "Boston Celtics",                54, 25, 40.5, "East",
  "2025-26", "New York Knicks",               51, 28, 53.5, "East",
  "2025-26", "Cleveland Cavaliers",           51, 29, 56.5, "East",
  "2025-26", "Atlanta Hawks",                 45, 35, 46.5, "East",
  "2025-26", "Toronto Raptors",               44, 35, 38.5, "East",
  "2025-26", "Orlando Magic",                 44, 36, 51.5, "East",
  "2025-26", "Philadelphia 76ers",            43, 36, 42.5, "East",
  "2025-26", "Charlotte Hornets",             43, 37, 28.5, "East",
  "2025-26", "Miami Heat",                    41, 38, 38.5, "East",
  "2025-26", "Milwaukee Bucks",               31, 49, 44.5, "East",
  "2025-26", "Chicago Bulls",                 30, 49, 31.5, "East",
  "2025-26", "Brooklyn Nets",                 20, 59, 20.5, "East",
  "2025-26", "Indiana Pacers",                18, 61, 38.5, "East",
  "2025-26", "Washington Wizards",            17, 62, 20.5, "East",
)

# =============================================================================
# Compute derived columns
# =============================================================================

analysis <- results %>%
  group_by(season, conference) %>%
  arrange(desc(wins), team) %>%
  mutate(conf_seed = row_number()) %>%
  ungroup() %>%
  mutate(
    over_under = if_else(wins > projection, "OVER", "UNDER"),
    differential = wins - projection,
    playoff_status = case_when(
      conf_seed <= 6  ~ "Playoffs",
      conf_seed <= 10 ~ "Play-In",
      TRUE            ~ "Eliminated"
    ),
    playoff_status = factor(playoff_status,
                            levels = c("Playoffs", "Play-In", "Eliminated"))
  )

# =============================================================================
# Cross-tab summary
# =============================================================================

cross_tab <- analysis %>%
  count(season, playoff_status, over_under) %>%
  pivot_wider(names_from = over_under, values_from = n, values_fill = 0) %>%
  mutate(
    total = OVER + UNDER,
    pct_over = round(OVER / total * 100, 1)
  ) %>%
  arrange(season, playoff_status)

print(cross_tab, n = 50)

# =============================================================================
# Plot 1: OVER % by playoff status across seasons
# =============================================================================

p1 <- cross_tab %>%
  ggplot(aes(x = season, y = pct_over, fill = playoff_status)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray50", linewidth = 0.4) +
  scale_fill_manual(
    values = c("Playoffs" = "#0F6E56", "Play-In" = "#185FA5", "Eliminated" = "#A32D2D"),
    name = NULL
  ) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(
    title = "Percentage of teams going OVER by playoff status",
    subtitle = "Five NBA seasons (2020-21 through 2025-26)",
    x = NULL, y = "% of group that went OVER"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

print(p1)

# =============================================================================
# Plot 2: Scatter of all teams across all seasons
# =============================================================================

p2 <- analysis %>%
  ggplot(aes(x = projection, y = wins, color = playoff_status, shape = playoff_status)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_color_manual(
    values = c("Playoffs" = "#0F6E56", "Play-In" = "#185FA5", "Eliminated" = "#A32D2D"),
    name = NULL
  ) +
  scale_shape_manual(values = c(16, 18, 17), name = NULL) +
  facet_wrap(~ season, nrow = 2) +
  labs(
    title = "Actual wins vs Vegas projection by playoff status",
    subtitle = "Each dot is one team. Above the dashed line = OVER.",
    x = "Vegas win projection", y = "Actual wins"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

print(p2)

# =============================================================================
# Plot 3: Heatmap of the cross-tab
# =============================================================================

p3 <- cross_tab %>%
  mutate(label = paste0(OVER, "/", total, "\n(", pct_over, "%)")) %>%
  ggplot(aes(x = season, y = fct_rev(playoff_status), fill = pct_over)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = label), size = 3.2, color = "white", fontface = "bold") +
  scale_fill_gradient2(
    low = "#A32D2D", mid = "#888780", high = "#0F6E56",
    midpoint = 50, name = "% OVER",
    labels = \(x) paste0(x, "%")
  ) +
  labs(
    title = "OVER/UNDER cross-tab: playoff status vs season",
    subtitle = "Cell shows: OVER count / total (OVER %)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )

print(p3)

# =============================================================================
# Plot 4: Biggest individual team misses per season
# =============================================================================

extremes <- analysis %>%
  group_by(season) %>%
  slice_max(abs(differential), n = 5) %>%
  ungroup() %>%
  mutate(
    short_team = str_extract(team, "\\w+$"),  # last word of team name
    label = paste0(short_team, " (", if_else(differential > 0, "+", ""),
                   differential, ")")
  )

p4 <- extremes %>%
  mutate(season = fct_rev(season)) %>%
  ggplot(aes(x = differential, y = reorder_within(label, abs(differential), season),
             fill = over_under)) +
  geom_col() +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  scale_fill_manual(values = c("OVER" = "#0F6E56", "UNDER" = "#A32D2D"), name = NULL) +
  scale_y_reordered() +
  facet_wrap(~ season, scales = "free_y", ncol = 1) +
  labs(
    title = "Biggest Vegas misses per season (top 5 by magnitude)",
    x = "Wins minus projection", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )

print(p4)

# =============================================================================
# Five-year aggregate summary
# =============================================================================

five_year <- analysis %>%
  count(playoff_status, over_under) %>%
  pivot_wider(names_from = over_under, values_from = n, values_fill = 0) %>%
  mutate(
    total = OVER + UNDER,
    pct_over = round(OVER / total * 100, 1)
  )

cat("\n=== Five-Year Aggregate ===\n")
print(five_year)
cat("\nKey finding: Playoff teams go OVER ~69% of the time.\n")
cat("Play-In teams split roughly 50/50 (except 2022-23 outlier at 12.5%).\n")
cat("Eliminated teams go UNDER ~84% of the time.\n")