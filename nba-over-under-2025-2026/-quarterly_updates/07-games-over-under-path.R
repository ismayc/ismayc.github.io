# Games ahead/behind projection pace throughout the season
# Source after 03-data-wrangling.R

library(tidyverse)

pace_df <- join_with_projections %>%
  mutate(
    expected_wins_at_pace = win_projection / num_games * `Game Number`,
    games_ahead = current_wins - expected_wins_at_pace
  ) %>%
  select(`Team Name`, `Game Number`, `Game Date`, current_wins,
         expected_wins_at_pace, games_ahead, win_projection, over_under)

# Clinch status per team
clinch_status_df <- projections %>%
  mutate(loss_projection = num_games - win_projection) %>%
  inner_join(
    standings %>% select(team_name, wins, losses),
    by = c("team" = "team_name")
  ) %>%
  mutate(wins = as.numeric(wins), losses = as.numeric(losses)) %>%
  transmute(
    `Team Name` = team,
    clinch_status = case_when(
      wins > win_projection  ~ "OVER",
      losses > loss_projection ~ "UNDER",
      TRUE ~ "not yet"
    )
  )

# Current pace for ordering the facets
latest_pace <- pace_df %>%
  group_by(`Team Name`) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  inner_join(clinch_status_df, by = "Team Name") %>%
  mutate(
    is_clinched = clinch_status != "not yet",
    pace_label = paste0(
      current_wins, "W  (",
      if_else(games_ahead >= 0, "+", ""),
      round(games_ahead, 1), ")"
    ),
    clinch_tag = case_when(
      clinch_status == "OVER"  ~ " \u2191",
      clinch_status == "UNDER" ~ " \u2193",
      TRUE ~ ""
    )
  ) %>%
  arrange(is_clinched, desc(games_ahead))

# Build facet labels
facet_labels <- latest_pace %>%
  mutate(facet_label = paste0(`Team Name`, clinch_tag, "\n", pace_label)) %>%
  select(`Team Name`, facet_label) %>%
  deframe()

pace_df <- pace_df %>%
  inner_join(clinch_status_df, by = "Team Name") %>%
  mutate(facet = factor(facet_labels[`Team Name`],
                        levels = facet_labels))

# Build background rects for clinched panels (split by OVER/UNDER)
y_range <- range(pace_df$games_ahead, na.rm = TRUE)
y_pad <- diff(y_range) * 0.05

clinched_over_bg <- pace_df %>%
  filter(clinch_status == "OVER") %>%
  distinct(facet) %>%
  mutate(xmin = -Inf, xmax = Inf,
         ymin = -Inf, ymax = Inf)

clinched_under_bg <- pace_df %>%
  filter(clinch_status == "UNDER") %>%
  distinct(facet) %>%
  mutate(xmin = -Inf, xmax = Inf,
         ymin = -Inf, ymax = Inf)

ggplot(pace_df, aes(x = `Game Number`, y = games_ahead)) +
  # Light background fill for clinched OVER panels
  geom_rect(
    data = clinched_over_bg,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "forestgreen",
    alpha = 0.3
  ) +
  # Light background fill for clinched UNDER panels
  geom_rect(
    data = clinched_under_bg,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "deeppink",
    alpha = 0.3
  ) +
  geom_hline(yintercept = 0, color = "blue", linewidth = 0.5) +
  geom_line(aes(color = over_under), linewidth = 0.6) +
  geom_area(aes(fill = games_ahead > 0), alpha = 0.15) +
  facet_wrap(~ facet, nrow = 6) +
  scale_color_manual(
    values = c("OVER" = "forestgreen", "UNDER" = "deeppink", "PUSH" = "black"),
    name = "Current Status"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "forestgreen", "FALSE" = "deeppink"),
    guide = "none"
  ) +
  scale_x_continuous(limits = c(1, num_games)) +
  labs(
    title = paste0("Games ahead/behind projected win pace (through ",
                   latest_date, ")"),
    subtitle = paste0(
      "Blue line = on pace. Above = ahead of projection. ",
      "Non-clinched first, clinched at bottom (green bg = OVER, pink bg = UNDER)."
    ),
    x = "Game Number",
    y = "Games Ahead/Behind Pace"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.spacing = unit(0.5, "lines"),
    panel.spacing.y = unit(1, "lines"),
    strip.text = element_text(size = 7),
    axis.text.x = element_text(size = 6),
    legend.position = "bottom"
  )
