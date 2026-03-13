
# =========================================================================
# 6c. HEATMAP -- clinched team x player
# =========================================================================

heatmap_clinch <- clinch_impact %>%
  mutate(
    Team_label = paste0(
      Team, " (", format(clinch_date, "%b %d"), ") ",
      if_else(as.character(outcome) == "OVER", "\u2191", "\u2193")
    )
  )

# Order teams by clinch date
team_order <- heatmap_clinch %>%
  distinct(Team_label, clinch_date) %>%
  arrange(clinch_date) %>%
  pull(Team_label)

# Order players by current ranking
heatmap_clinch <- heatmap_clinch %>%
  mutate(
    Team_label = factor(Team_label, levels = rev(team_order)),
    player = factor(player, levels = current_rankings)
  )

ggplot(heatmap_clinch, aes(x = player, y = Team_label, fill = points)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = points), size = 3.5, fontface = "bold") +
  scale_fill_gradient2(
    low = "#c51b7d", mid = "white", high = "#4d9221",
    midpoint = 0,
    name = "Points"
  ) +
  labs(
    title = "Point impact of each clinched outcome by player",
    subtitle = "Teams ordered by clinch date (earliest at top). Players ordered by current rank.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9)
  )


# =========================================================================
# 6d. GROUPED BAR CHART -- per clinch event
# =========================================================================

bar_clinch <- clinch_impact %>%
  mutate(
    Team_label = paste0(
      Team, "\n(", format(clinch_date, "%b %d"), ")"
    )
  )

# Order teams by clinch date
bar_team_order <- bar_clinch %>%
  distinct(Team_label, clinch_date) %>%
  arrange(clinch_date) %>%
  pull(Team_label)

bar_clinch <- bar_clinch %>%
  mutate(
    Team_label = factor(Team_label, levels = bar_team_order),
    player = factor(player, levels = current_rankings)
  )

ggplot(bar_clinch, aes(x = player, y = points, fill = points > 0)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(
      label = if_else(points > 0, paste0("+", points), as.character(points)),
      vjust = if_else(points > 0, -0.3, 1.3)
    ),
    size = 2.3
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
  facet_wrap(~ Team_label, nrow = 2) +
  scale_fill_manual(
    values = c("TRUE" = "forestgreen", "FALSE" = "firebrick"),
    guide = "none"
  ) +
  labs(
    title = "Point impact per player for each clinched team",
    subtitle = "Teams ordered left to right by clinch date. Players ordered by current rank.",
    x = NULL,
    y = "Points"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.major.x = element_blank()
  )


# =========================================================================
# 6e. LOLLIPOP CHART -- per clinch event
# =========================================================================

lollipop_clinch <- clinch_impact %>%
  mutate(
    Team_label = paste0(
      Team, "\n(", format(clinch_date, "%b %d"), ")"
    )
  )

lollipop_team_order <- lollipop_clinch %>%
  distinct(Team_label, clinch_date) %>%
  arrange(clinch_date) %>%
  pull(Team_label)

lollipop_clinch <- lollipop_clinch %>%
  mutate(
    Team_label = factor(Team_label, levels = lollipop_team_order)
  )

ggplot(
  lollipop_clinch,
  aes(x = points, y = reorder(player, points))
) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey40") +
  geom_segment(
    aes(x = 0, xend = points, yend = player),
    linewidth = 0.9,
    color = "grey70"
  ) +
  geom_point(
    aes(color = points > 0),
    size = 3
  ) +
  geom_text(
    aes(
      label = if_else(points > 0, paste0("+", points), as.character(points)),
      hjust = if_else(points > 0, -0.4, 1.4)
    ),
    size = 3
  ) +
  facet_wrap(~ Team_label, nrow = 2) +
  scale_color_manual(
    values = c("TRUE" = "forestgreen", "FALSE" = "firebrick"),
    guide = "none"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.3, 0.3))) +
  labs(
    title = "Point impact per player for each clinched team",
    subtitle = "Teams ordered left to right by clinch date",
    x = "Points",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    panel.grid.major.y = element_blank()
  )