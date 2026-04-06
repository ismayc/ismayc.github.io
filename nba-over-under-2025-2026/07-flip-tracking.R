# 07-flip-tracking.R
# Run after 03-data-wrangling.R
# Requires: standings_grid, picks, projections, player_projections_by_team, num_games

library(tidyverse)
library(plotly)
library(glue)

source("03-data-wrangling.R")

# =========================================================================
# 1. Build daily over/under + clinch status per team
# =========================================================================

# Compute Outcome Determined at the team level for every date
# (player_projections_by_team has it but is crossed with players;
#  we need one row per team per date)

team_daily <- standings_grid %>%
  filter(!is.na(over_under)) %>%
  group_by(team) %>%
  arrange(date) %>%
  mutate(
    games_played = current_wins + current_losses,
    remaining = num_games - games_played,
    wins_to_go_over = ceiling(
      `Vegas Win Projection %` / 100 * num_games
    ) - current_wins,
    losses_to_go_under = num_games - ceiling(
      `Vegas Win Projection %` / 100 * num_games
    ) - current_losses,
    clinch_status = case_when(
      wins_to_go_over <= 0   ~ "OVER",
      remaining < wins_to_go_over ~ "UNDER",
      TRUE                    ~ "not yet"
    ),
    # Detect the date each team clinched
    just_clinched = clinch_status != "not yet" &
      (lag(clinch_status, default = "not yet") == "not yet"),
    # Detect flips (only meaningful while still "not yet")
    status_changed = over_under != lag(over_under) &
      clinch_status == "not yet" &
      lag(clinch_status, default = "not yet") == "not yet",
    status_changed = replace_na(status_changed, FALSE),
    flip_number = cumsum(status_changed)
  ) %>%
  ungroup()

# =========================================================================
# 2. Summary tables
# =========================================================================

# When did each team clinch? (NA if not yet)
clinch_dates <- team_daily %>%
  filter(just_clinched) %>%
  group_by(team) %>%
  slice_min(date, n = 1) %>%
  ungroup() %>%
  select(team, clinch_date = date, clinched_as = clinch_status)

# Flip counts + current/clinch status
flip_summary <- team_daily %>%
  group_by(team) %>%
  summarize(
    total_flips = sum(status_changed, na.rm = TRUE),
    current_over_under = last(over_under),
    .groups = "drop"
  ) %>%
  left_join(clinch_dates, by = "team") %>%
  left_join(
    projections %>% select(team, win_projection),
    by = "team"
  ) %>%
  mutate(
    display_status = case_when(
      !is.na(clinched_as) ~ paste0(clinched_as, " (clinched)"),
      TRUE                ~ current_over_under
    )
  ) %>%
  arrange(desc(total_flips), team)

cat("\n=== Flip summary (all teams) ===\n")
print(flip_summary %>% select(team, total_flips, display_status, clinch_date),
      n = 30)

# Split into clinched vs. still live
clinched_teams <- flip_summary %>% filter(!is.na(clinched_as))
live_teams <- flip_summary %>% filter(is.na(clinched_as))

cat(glue("\n{nrow(clinched_teams)} teams clinched, ",
         "{nrow(live_teams)} still live\n\n"))

# =========================================================================
# 3. Tile heatmap: status over time with clinch markers
# =========================================================================

# Order: clinched at bottom of plot, live at top
# (ggplot plots factor level 1 at the bottom of the y-axis)
team_order <- bind_rows(
  clinched_teams %>% arrange(total_flips),
  live_teams %>% arrange(total_flips)
) %>%
  pull(team)

tile_data <- team_daily %>%
  mutate(
    team = factor(team, levels = team_order),
    tile_status = case_when(
      clinch_status == "OVER"  ~ "Clinched OVER",
      clinch_status == "UNDER" ~ "Clinched UNDER",
      over_under == "OVER"     ~ "OVER",
      over_under == "UNDER"    ~ "UNDER",
      TRUE                     ~ "PUSH"
    )
  )

# Four-color scale: bright for live, muted for clinched
status_colors <- c(
  "OVER"           = "forestgreen",
  "UNDER"          = "deeppink",
  "Clinched OVER"  = "#2d6a2d",
  "Clinched UNDER" = "#8b2252",
  "PUSH"           = "grey70"
)

flip_tile_plot <- ggplot(tile_data,
                         aes(x = date, y = team, fill = tile_status,
                             text = paste0(team, "\n", date, "\n", tile_status,
                                           "\nRecord: ", current_wins, "-", current_losses))) +
  geom_tile(height = 0.85) +
  # Mark clinch dates with a white dot
  geom_point(
    data = tile_data %>% filter(just_clinched),
    aes(x = date, y = team),
    color = "white", size = 1.5, shape = 18, inherit.aes = FALSE
  ) +
  scale_fill_manual(values = status_colors, name = "Status") +
  # Divider between clinched and live groups
  geom_hline(
    yintercept = nrow(clinched_teams) + 0.5,
    linetype = "dashed", color = "grey50", linewidth = 0.4
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Over/Under status throughout the season (still to clinch is above the dotted line, clinched is below)",
    subtitle = glue(
      "White diamonds = clinch date | ",
      "Darker shades = clinched (locked in) | ",
      "Sorted by flip count within group"
    ),
    x = NULL, y = NULL
  )

ggplotly(flip_tile_plot, tooltip = "text") %>%
  layout(
    margin = list(t = 80),
    legend = list(orientation = "h", y = -0.12)
  )

# =========================================================================
# 4. Flip bar chart with clinch annotations
# =========================================================================

flip_bar_data <- flip_summary %>%
  mutate(
    team = fct_reorder(team, total_flips),
    bar_fill = case_when(
      clinched_as == "OVER"  ~ "Clinched OVER",
      clinched_as == "UNDER" ~ "Clinched UNDER",
      current_over_under == "OVER"  ~ "OVER",
      current_over_under == "UNDER" ~ "UNDER"
    ),
    label = if_else(
      !is.na(clinched_as),
      paste0(total_flips, " (", clinched_as, " \u2713)"),
      as.character(total_flips)
    )
  )

flip_bar <- ggplot(flip_bar_data,
                   aes(x = total_flips, y = team, fill = bar_fill)) +
  geom_col() +
  geom_text(aes(label = label), hjust = -0.15, size = 2.8) +
  scale_fill_manual(values = status_colors, name = "Current Status") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  theme_minimal() +
  labs(
    title = "Total over/under flips per team",
    subtitle = "Checkmark = outcome clinched (no more flips possible)",
    x = "Total flips", y = NULL
  )

flip_bar

# =========================================================================
# 5. Margin from line (who might flip next?)
# =========================================================================

margin_from_line <- team_daily %>%
  filter(date == max(date)) %>%
  left_join(clinch_dates, by = "team") %>%
  mutate(
    margin = abs(`Current Win %` - `Vegas Win Projection %`),
    can_still_flip = is.na(clinched_as)
  ) %>%
  select(team, `Current Win %`, `Vegas Win Projection %`,
         over_under, margin, can_still_flip, clinched_as) %>%
  arrange(desc(can_still_flip), margin)

cat("\n=== Teams closest to their line (live teams first) ===\n")
print(margin_from_line %>% head(15))

# =========================================================================
# 6. Points impact: which team flips have moved the standings most?
# =========================================================================

flip_dates_df <- team_daily %>%
  filter(status_changed) %>%
  select(team, date, over_under)

# Also include the clinch event itself as a "final lock-in"
clinch_events <- team_daily %>%
  filter(just_clinched) %>%
  select(team, date, over_under = clinch_status) %>%
  mutate(event_type = "clinch")

flip_events <- flip_dates_df %>%
  mutate(event_type = "flip")

all_events <- bind_rows(flip_events, clinch_events) %>%
  arrange(team, date)

# Cross with picks for impact scoring
event_impact <- all_events %>%
  inner_join(picks, by = "team") %>%
  mutate(
    swing = case_when(
      over_under == choice  ~  2 * wage,
      over_under != choice  ~ -2 * wage
    )
  )

team_impact_score <- event_impact %>%
  group_by(team) %>%
  summarize(
    total_events = n_distinct(date),
    flips = sum(event_type == "flip") / n_distinct(player),
    clinched = any(event_type == "clinch"),
    total_abs_swing = sum(abs(swing)),
    avg_swing_per_event = round(total_abs_swing / total_events, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(total_abs_swing))

cat("\n=== Teams causing the most total point disruption ===\n")
cat("(includes both flips and clinch events)\n")
print(team_impact_score)

# =========================================================================
# 7. Per-team flip timeline (sparkline-style for Rmd)
# =========================================================================

# A compact view showing each team's journey
team_journey <- team_daily %>%
  filter(!is.na(over_under)) %>%
  select(team, date, over_under, clinch_status, games_played,
         current_wins, current_losses, `Current Win %`,
         `Vegas Win Projection %`) %>%
  mutate(
    margin = `Current Win %` - `Vegas Win Projection %`,
    display = case_when(
      clinch_status != "not yet" ~ paste0("Clinched ", clinch_status),
      TRUE ~ over_under
    )
  )

# Small multiples: margin from line over time, colored by status
margin_plot <- ggplot(team_journey,
                      aes(x = date, y = margin, color = display)) +
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue",
             linewidth = 0.3) +
  facet_wrap(~ team, nrow = 6, scales = "free_y") +
  scale_color_manual(values = c(
    "OVER" = "forestgreen", "UNDER" = "deeppink",
    "Clinched OVER" = "#2d6a2d", "Clinched UNDER" = "#8b2252"
  )) +
  theme_bw(base_size = 9) +
  theme(
    strip.text = element_text(size = 7),
    legend.position = "bottom",
    panel.spacing = unit(0.3, "lines")
  ) +
  labs(
    title = "Win % margin above/below Vegas projection",
    subtitle = "Blue dashed line = projection. Above = OVER, below = UNDER.",
    x = NULL, y = "Win % - Projection %", color = "Status"
  )

margin_plot

# =========================================================================
# 8. Save for use in make_plots.Rmd
# =========================================================================

write_rds(flip_summary, here::here("rds", "flip_summary.rds"))
write_rds(clinch_dates, here::here("rds", "clinch_dates.rds"))
write_rds(team_impact_score, here::here("rds", "team_impact_score.rds"))
write_rds(margin_from_line, here::here("rds", "margin_from_line.rds"))