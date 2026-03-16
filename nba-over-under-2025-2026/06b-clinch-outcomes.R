# =========================================================================
# 6b. FACETED STEP CHART -- one panel per player
# =========================================================================

source("06-season-progression-plots.R")

clinch_cumulative_simple <- clinch_impact %>%
  arrange(clinch_date) %>%
  group_by(player) %>%
  mutate(cum_clinch_points = cumsum(points)) %>%
  ungroup()

# Add origin row per player
origin_rows <- clinch_cumulative_simple %>%
  distinct(player) %>%
  mutate(
    Team = NA_character_,
    clinch_date = min(clinch_impact$clinch_date) - 1,
    outcome = NA,
    points = 0,
    correct = NA,
    cum_clinch_points = 0
  )

clinch_faceted <- bind_rows(origin_rows, clinch_cumulative_simple) %>%
  arrange(player, clinch_date) %>%
  mutate(player = factor(player, levels = current_rankings))

# Point labels at each step (only for actual clinch events)
step_labels <- clinch_faceted %>%
  filter(!is.na(Team)) %>%
  mutate(
    point_label = if_else(points > 0, paste0("+", points), as.character(points))
  )

# Final total per player (rightmost point)
final_totals <- clinch_cumulative_simple %>%
  group_by(player) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    player = factor(player, levels = current_rankings),
    total_label = if_else(
      cum_clinch_points > 0,
      paste0("= +", cum_clinch_points),
      paste0("= ", cum_clinch_points)
    )
  )

# Build clinch reference lines with numbered events
clinch_ref_lines <- clinch_dates %>%
  arrange(clinch_date) %>%
  mutate(
    team_short = str_extract(Team, "\\w+$"),
    label = paste0(team_short, " (", format(clinch_date, "%b %d"), ")"),
    event_num = row_number()
  )

# Build numbered markers, collapsing same-date events and staggering vertically
clinch_ref_markers <- clinch_ref_lines %>%
  group_by(clinch_date) %>%
  summarize(
    marker = paste(event_num, collapse = ", "),
    event_order = min(event_num),
    .groups = "drop"
  ) %>%
  arrange(event_order) %>%
  mutate(row = row_number())

# Unique dates for vertical lines
clinch_ref_dates <- clinch_ref_lines %>%
  distinct(clinch_date)

# Build subtitle key string like "1=Suns (Feb 10)  2=Pacers (Feb 24) ..."
key_string <- clinch_ref_lines %>%
  mutate(entry = paste0(event_num, "=", team_short, " (", format(clinch_date, "%b %d"), ")")) %>%
  pull(entry) %>%
  paste(collapse = "   ")

# Two y levels for staggering markers
y_max <- max(clinch_faceted$cum_clinch_points, na.rm = TRUE)
y_min <- min(clinch_faceted$cum_clinch_points, na.rm = TRUE)
y_range <- y_max - y_min
marker_y_low  <- y_min - y_range * 0.2
marker_y_high <- y_max + y_range * 0.3

clinch_ref_markers <- clinch_ref_markers %>%
  mutate(marker_y = if_else(row %% 2 == 1, marker_y_low, marker_y_high))

ggplot(clinch_faceted, aes(x = clinch_date, y = cum_clinch_points)) +
  # Dotted vertical reference lines at each clinch date
  geom_vline(
    data = clinch_ref_dates,
    aes(xintercept = clinch_date),
    linetype = "dotted",
    color = "grey70",
    linewidth = 0.3
  ) +
  # Numbered markers staggered at two heights
  geom_label(
    data = clinch_ref_markers,
    aes(x = clinch_date, y = marker_y, label = marker),
    inherit.aes = FALSE,
    size = 3,
    color = "grey25",
    fill = "grey95",
    label.padding = unit(0.1, "lines"),
    label.size = 0.2,
    fontface = "bold"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_step(aes(color = player), linewidth = 0.9, show.legend = FALSE) +
  geom_point(
    data = step_labels,
    aes(color = player),
    size = 2, show.legend = FALSE
  ) +
  # Signed point value, repelled from the dot
  geom_label_repel(
    data = step_labels,
    aes(label = point_label, color = player),
    size = 2.3,
    fill = "white",
    alpha = 0.9,
    label.padding = unit(0.1, "lines"),
    label.r = unit(0.12, "lines"),
    direction = "y",
    nudge_y = 2,
    segment.color = "grey60",
    segment.size = 0.2,
    segment.alpha = 0.4,
    min.segment.length = 0,
    box.padding = 0.2,
    point.padding = 0.2,
    force = 2,
    max.overlaps = Inf,
    show.legend = FALSE,
    seed = 42
  ) +
  # Total label with arrow pointing at the rightmost point
  geom_label(
    data = final_totals,
    aes(x = clinch_date + 3, y = cum_clinch_points, label = total_label,
        color = player),
    size = 3,
    fontface = "bold",
    fill = "white",
    label.padding = unit(0.15, "lines"),
    linewidth = 0.3,
    hjust = 0,
    show.legend = FALSE
  ) +
  geom_segment(
    data = final_totals,
    aes(x = clinch_date + 2.5, xend = clinch_date + 0.5,
        y = cum_clinch_points, yend = cum_clinch_points,
        color = player),
    arrow = arrow(length = unit(0.12, "inches"), type = "closed"),
    linewidth = 0.6,
    show.legend = FALSE
  ) +
  facet_wrap(~ player, nrow = 2) +
  scale_color_manual(values = player_colors) +
  scale_x_date(expand = expansion(mult = c(0.02, 0.15))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Cumulative points from clinched outcomes (by player)",
    subtitle = key_string,
    x = NULL,
    y = "Cumulative Points"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.subtitle = element_text(size = 8, color = "grey35")
  )
