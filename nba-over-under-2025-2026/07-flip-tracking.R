# 07-flip-tracking.R
# Run after 03-data-wrangling.R (which creates standings_grid and join_with_projections)

library(tidyverse)
library(plotly)
library(here)
library(glue)

# ---- 1. Count flips per team from the daily standings grid ----

standings_grid <- read_rds(
  here("rds", glue("standings_grid_through_{Sys.Date() - 1}.rds")))

team_flips <- standings_grid %>%
  filter(!is.na(over_under)) %>%
  group_by(team) %>%
  arrange(date) %>%
  mutate(
    status_changed = over_under != lag(over_under),
    flip_number = cumsum(replace_na(status_changed, FALSE))
  ) %>%
  ungroup()

flip_summary <- team_flips %>%
  group_by(team) %>%
  summarize(
    total_flips = sum(status_changed, na.rm = TRUE),
    current_status = last(over_under),
    .groups = "drop"
  ) %>%
  arrange(desc(total_flips), team)

cat("\n=== Teams by number of over/under flips ===\n")
print(flip_summary, n = 30)

# ---- 2. Timeline of each team's over/under status ----
# This shows WHEN each flip happened

flip_dates <- team_flips %>%
  filter(status_changed == TRUE) %>%
  select(team, date, over_under) %>%
  arrange(team, date)

# ---- 3. Which teams are "wobbly" (close to the line)? ----
# Teams with the smallest margin between current win% and projected win%

margin_from_line <- standings_grid %>%
  filter(!is.na(over_under), date == max(date)) %>%
  mutate(
    margin = abs(`Current Win %` - `Vegas Win Projection %`)
  ) %>%
  select(team, `Current Win %`, `Vegas Win Projection %`, 
         over_under, margin) %>%
  arrange(margin)

cat("\n=== Teams closest to their projection line (most likely to flip next) ===\n")
print(margin_from_line %>% head(10))

# ---- 4. Flip timeline plot (tile/heatmap style) ----

# Order teams by total flips (most at top)
team_order <- flip_summary %>%
  arrange(total_flips) %>%
  pull(team)

flip_plot_data <- team_flips %>%
  filter(!is.na(over_under)) %>%
  mutate(team = factor(team, levels = team_order))

flip_tile_plot <- ggplot(
  flip_plot_data,
  aes(x = date, y = team, fill = over_under)
) +
  geom_tile(height = 0.8) +
  scale_fill_manual(
    values = c("OVER" = "forestgreen", "UNDER" = "deeppink"),
    name = "Status"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Over/Under status throughout the season",
    subtitle = "Teams sorted by total number of flips (most flips at top)",
    x = "Date", y = NULL
  )

# Interactive version
ggplotly(flip_tile_plot, tooltip = c("team", "date", "over_under"))

# ---- 5. Flip count bar chart ----

flip_bar <- flip_summary %>%
  mutate(team = fct_reorder(team, total_flips)) %>%
  ggplot(aes(x = total_flips, y = team, fill = current_status)) +
  geom_col() +
  geom_text(aes(label = total_flips), hjust = -0.3, size = 3) +
  scale_fill_manual(
    values = c("OVER" = "forestgreen", "UNDER" = "deeppink"),
    name = "Current Status"
  ) +
  theme_minimal() +
  labs(
    title = "Number of over/under status flips per team",
    x = "Total flips", y = NULL
  )

flip_bar

# ---- 6. Points impact: which teams' flips have mattered most? ----
# Cross-reference flips with player wagers to find "high-impact" flippers

flip_impact <- flip_dates %>%
  inner_join(picks, by = "team") %>%
  mutate(
    swing = case_when(
      over_under == choice ~ 2 * wage,
      over_under != choice ~ -2 * wage
    )
  )

# Total absolute swing caused by each team's flips across all players
team_impact_score <- flip_impact %>%
  group_by(team) %>%
  summarize(
    total_flips = n_distinct(date),
    total_abs_swing = sum(abs(swing)),
    avg_swing_per_flip = round(total_abs_swing / total_flips, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(total_abs_swing))

cat("\n=== Teams causing the most total point disruption ===\n")
print(team_impact_score)

# ---- 7. Save for use in make_plots.Rmd ----

write_rds(flip_summary, here::here("rds", "flip_summary.rds"))
write_rds(flip_dates, here::here("rds", "flip_dates.rds"))
write_rds(team_impact_score, here::here("rds", "team_impact_score.rds"))