library(dplyr)
library(readr)
library(stringr)

# 1. Read the tidy CSV
picks <- read_csv("picks.csv")
# columns: team, player, wage, choice (e.g. "OVER" / "UNDER")

# 2. Create signed confidence: OVER = +wage, UNDER = -wage
picks_signed <- picks |>
  mutate(
    choice = str_to_lower(choice),
    signed = case_when(
      choice == "over"  ~  wage,
      choice == "under" ~ -wage,
      TRUE              ~ NA_real_
    )
  )

# 3. Compute mean signed confidence per team across all players
picks_signed <- picks_signed |>
  group_by(team) |>
  mutate(
    mean_signed = mean(signed, na.rm = TRUE)
  ) |>
  ungroup()

# 4. Deviation from the group for each player and team
picks_signed <- picks_signed |>
  mutate(
    dev = abs(signed - mean_signed)
  )

# 5. For each player, get their top 10 "most different" teams
top_by_player <- picks_signed |>
  group_by(player) |>
  slice_max(order_by = dev, n = 5, with_ties = FALSE) |>
  arrange(player, desc(dev), team)

top_by_player

#6. Link to current_over_under
current_over_under <- read_csv("current_over_under.csv")

top_by_player_joined <- top_by_player |>
  left_join(
    current_over_under,
    by = c("team" = "Team")
  ) |> 
  mutate(choice = str_to_upper(choice)) |> 
  mutate(mismatch = if_else(choice == `Current Over/Under`, FALSE, TRUE))

# 7. For each player, count how many mismatches they have
mismatch_count <- top_by_player_joined |>
  group_by(player) |>
  summarize(
    mismatch_count = sum(mismatch, na.rm = TRUE),
    total_count = n(),
    mismatch_rate = mismatch_count / total_count
  ) |>
  arrange(mismatch_rate, mismatch_count, player)

# 8. Plots
library(dplyr)
library(forcats)
library(ggplot2)

plot_data <- top_by_player_joined |>
  group_by(player) |>
  mutate(
    team = fct_reorder(team, dev, .desc = TRUE)
  ) |>
  ungroup()

ggplot(plot_data, aes(x = team, y = dev, fill = mismatch)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "FALSE" = "#004CFF",   # vivid blue
      "TRUE"  = "#CC0000"    # vivid red
    )
  ) +
  coord_flip() +
  facet_wrap(~ player, scales = "free_y") +
  labs(
    title = "Most differentiating teams per player",
    x = "Team",
    y = "Absolute deviation from group",
    fill = "Mismatch with actual"
  ) +
  theme_minimal()

#######
library(tidytext)

# Current order of players
# player_order <- c(
#   "Mary",
#   "Chester",
#   "Phil",
#   "Mike",
#   "Adonis",
#   "Ryan",
#   "Andy",
#   "Jake"
# )

player_order <- c(
  "Jake",
  "Andy",
  "Chester",
  "Mike",
  "Ryan",
  "Mary",
  "Phil",
  "Adonis"
)

plot_data2 <- top_by_player_joined |>
  mutate(
    player = factor(player, levels = player_order),
    team_reordered = reorder_within(team, dev, player)
  )

ggplot(plot_data2, aes(x = team_reordered, y = dev, fill = mismatch)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "FALSE" = "#004CFF",
      "TRUE"  = "#CC0000"
    )
  ) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ player, nrow = 4, scales = "free_y") +
  labs(
    title = "Most differentiating teams per player",
    x = "Team",
    y = "Absolute deviation from group",
    fill = "Mismatch with actual"
  ) +
  theme_minimal()


#####


plot_data3 <- top_by_player_joined |>
  mutate(
    player = factor(player, levels = player_order),
    team_reordered = reorder_within(team, dev, player),
    label = paste0(wage, " ", str_to_title(choice))
  ) |> 
  mutate(player = case_when(
    player == "Mary"   ~ "6. Mary",
    player == "Chester" ~ "3. Chester",
    player == "Phil"   ~ "7. Phil",
    player == "Mike"   ~ "4. Mike",
    player == "Adonis" ~ "8. Adonis",
    player == "Ryan"   ~ "5. Ryan",
    player == "Andy"   ~ "2. Andy",
    player == "Jake"   ~ "1. Jake"
  ))

ggplot(plot_data3, aes(x = team_reordered, y = dev, fill = mismatch)) +
  geom_col() +
  geom_text(
    aes(label = label),
    color = "white",
    hjust = 1.1,     # push slightly inside the bar
    size = 3.2
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#004CFF",
      "TRUE"  = "#CC0000"
    )
  ) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ player, nrow = 4, scales = "free_y") +
  labs(
    title = "Most differentiating teams per player (At Halfway Point)",
    x = "Team",
    y = "Absolute deviation from group",
    fill = "Mismatch with actual"
  ) +
  theme_minimal()

# Also identify how things have changed since previous updates
# Quarter 1 was sent out on 2025-12-01
# Quarter 2 was sent out on 2026-01-15

projections <- read_csv("projections.csv") |> 
  mutate(win_proj_perc = win_projection / 82)

q1_standings <- read_rds("rds/standings_through_2025-11-30.rds") |> 
  select(team_name, conference, wins, losses, `Winning Pct`) |> 
  inner_join(projections |> select(-win_projection), 
             by = c("team_name" = "team", "conference")) |> 
  rename("Team" = "team_name") |> 
  mutate("Q1 Over/Under" = if_else(`Winning Pct` > win_proj_perc, "OVER", "UNDER"))
  
#q2_standings <- read_rds("rds/standings_through_2026-01-15.rds")
q2_standings <- read_rds("rds/standings_through_2026-01-14.rds") |> 
  select(team_name, conference, wins, losses, `Winning Pct`) |> 
  inner_join(projections |> select(-win_projection), 
             by = c("team_name" = "team", "conference")) |> 
  rename("Team" = "team_name") |> 
  mutate("Q2 Over/Under" = if_else(`Winning Pct` > win_proj_perc, "OVER", "UNDER"))

# Compare Q1 to Q2
q1_q2_compare <- q1_standings |>
  select(Team, `Q1 Over/Under`) |>
  inner_join(
    q2_standings |> select(Team, `Q2 Over/Under`),
    by = "Team"
  ) |> 
  mutate(
    changed = if_else(`Q1 Over/Under` == `Q2 Over/Under`, FALSE, TRUE)
  ) |> 
  arrange(desc(changed), Team)

picks_signed_with_q_status <- picks_signed |>
  left_join(
    current_over_under,
    by = c("team" = "Team")
  ) |> 
  mutate(choice = str_to_upper(choice)) |> 
  mutate(current_mismatch = if_else(choice == `Current Over/Under`, FALSE, TRUE)) |>
  left_join(
    q1_q2_compare |> select(-changed),
    by = c("team" = "Team")
  ) |> 
  mutate(q1_mismatch = if_else(choice == `Q1 Over/Under`, FALSE, TRUE),
         .after = `Q1 Over/Under`) |>
  mutate(q2_mismatch = if_else(choice == `Q2 Over/Under`, FALSE, TRUE),
         .after = `Q2 Over/Under`
  ) |> 
  mutate(q1_signed = case_when(
    `Q1 Over/Under` == choice  ~ wage,
    `Q1 Over/Under` != choice ~ -wage,
    TRUE                       ~ NA_real_
  ), .after = q1_mismatch) |> 
  mutate(q2_signed = case_when(
    `Q2 Over/Under` == choice  ~ wage,
    `Q2 Over/Under` != choice ~ -wage,
    TRUE                       ~ NA_real_
  ), .after = q2_mismatch) |> 
  mutate(current_signed = case_when(
    `Current Over/Under` == choice  ~ wage,
    `Current Over/Under` != choice ~ -wage,
    TRUE                          ~ NA_real_
  ), .after = current_mismatch) |> 
  mutate(
    point_change_q1_to_q2 = q2_signed - q1_signed,
    point_change_q2_to_current = current_signed - q2_signed
  )

point_total_progression <- picks_signed_with_q_status |> 
  group_by(player) |> 
  summarize(q1_score = sum(q1_signed),
            num_q1_correct = sum(q1_mismatch == FALSE),
            q2_score = sum(q2_signed),
            num_q2_correct = sum(q2_mismatch == FALSE),
            current_score = sum(current_signed),
            num_current_correct = sum(current_mismatch == FALSE))

# Count how changes in points happened per player based on
# team changes from Q1 to Q2 to Current
q2_changes <- picks_signed_with_q_status  |> 
  filter(point_change_q1_to_q2 != 0) |>
  select(team, player, point_change_q1_to_q2)

# Overall changes from q1 to q2
q2_changes_summary <- q2_changes |>
  group_by(player) |>
  summarize(
    total_point_change_q1_to_q2 = sum(point_change_q1_to_q2),
    num_teams_changed = n()
  ) |>
  arrange(desc(total_point_change_q1_to_q2), player)

## Faceted “lollipop” chart (great for per-team, per-player comparisons)

# Order teams by average change
q2_changes_ordered <- q2_changes %>%
  group_by(team) %>%
  mutate(team_avg = mean(point_change_q1_to_q2)) %>%
  ungroup() %>%
  mutate(team = reorder(team, team_avg))

ggplot(
  q2_changes_ordered,
  aes(
    x = point_change_q1_to_q2,
    y = reorder(player, point_change_q1_to_q2)
  )
) +
  # zero reference line
  geom_vline(xintercept = 0, linewidth = 0.6, color = "grey70") +
  
  # lollipop stem
  geom_segment(
    aes(x = 0, xend = point_change_q1_to_q2, yend = player),
    linewidth = 1.1,
    color = "grey70"
  ) +
  
  # lollipop head
  geom_point(
    aes(color = point_change_q1_to_q2 > 0),
    size = 3
  ) +
  
  # value labels with + sign
  geom_text(
    aes(
      label = if_else(
        point_change_q1_to_q2 > 0,
        paste0("+", point_change_q1_to_q2),
        as.character(point_change_q1_to_q2)
      ),
      hjust = if_else(point_change_q1_to_q2 > 0, -0.45, 1.45)
    ),
    size = 3.4
  ) +
  
  facet_wrap(~ team, scales = "free_y") +
  
  scale_color_manual(
    values = c("TRUE" = "forestgreen", "FALSE" = "firebrick"),
    guide = "none"
  ) +
  
  # extra space so labels do not clip
  scale_x_continuous(
    expand = expansion(mult = c(0.2, 0.25))
  ) +
  
  labs(
    title = "Player point change from Q1 to Q2 by team",
    x = "\nPoint change (Q1 to Q2)",
    y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Show how player rankings have changed in terms of total points
# from Q1 to Q2 to current with rankings from 1 to 8
point_total_progression_ranked <- point_total_progression |>
  mutate(
    rank_q1 = min_rank(desc(q1_score)),
    rank_q2 = min_rank(desc(q2_score)),
    rank_current = min_rank(desc(current_score))
  ) |>
  select(player, starts_with("rank_"), starts_with("q"), current_score) |>
  mutate(rank_q1 = if_else(player == "Ryan", 6, rank_q1)) |> 
  arrange(rank_current)
point_total_progression_ranked

# Plot it
library(tidyverse)
library(ggrepel)

score_slope_df <- point_total_progression_ranked %>%
  mutate(
    score_change = q2_score - q1_score,
    same_rank = rank_q1 == rank_q2,
    lty = case_when(
      same_rank & score_change < 0 ~ "same rank, score down",
      same_rank & score_change > 0 ~ "same rank, score up",
      TRUE ~ "rank changed"
    ),
    label = paste0(player, " (", rank_q1, " → ", rank_q2, ")")
  ) %>%
  transmute(
    label,
    x1 = "Q1", y1 = q1_score,
    x2 = "Q2", y2 = q2_score,
    q1_score,
    q2_score,
    score_change,
    lty
  )

ggplot(score_slope_df) +
  # legend-driving layer (plain lines, no arrows)
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2, linetype = lty),
    linewidth = 1.2, color = "grey35", show.legend = TRUE
  ) +
  # visual layer (colored arrows, no legend)
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2, color = score_change, linetype = lty),
    arrow = arrow(length = unit(0.18, "inches")),
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  
  # Q1 score bubble (slightly faded so Q2 pops more)
  geom_label(
    aes(x = x1, y = y1, label = q1_score),
    nudge_x = -0.03,
    size = 2.6,
    linewidth = 0.25,
    label.padding = unit(0.14, "lines"),
    label.r = unit(0.2, "lines"),   # rounder corners
    fill = "grey98",
    alpha = 0.75,                    # fade Q1 bubble
    color = "grey30"
  ) +
  
  # player labels (repelled), with lighter connectors
  geom_text_repel(
    aes(x = x2, y = y2, label = label),
    nudge_x = 0.12,
    hjust = 0,
    direction = "y",
    box.padding = 0.25,
    point.padding = 0.4,             # extra space so connectors avoid the Q2 bubble more
    force = 2,
    min.segment.length = 0.2,
    segment.alpha = 0.25,
    segment.size = 0.25,
    seed = 123,
    size = 4
  ) +

  # Q2 score bubble (crisper / more prominent)
  geom_label(
    aes(x = x2, y = y2, label = q2_score),
    nudge_x = 0.03,
    size = 2.6,
    linewidth = 0.25,
    label.padding = unit(0.25, "lines"),
    label.r = unit(0.2, "lines"),   # rounder corners
    fill = "white",
    alpha = 1,
    color = "grey20"
  ) +  
    
  scale_color_gradient2(
    low = "firebrick4", mid = "grey85", high = "springgreen3",
    midpoint = 0, name = "Score change\n(Q2 - Q1)"
  ) +
  scale_linetype_manual(
    values = c(
      "rank changed" = "solid",
      "same rank, score down" = "dotted",
      "same rank, score up" = "dashed"
    ),
    name = NULL
  ) +
  scale_x_discrete(expand = expansion(add = c(0.1, 0.55))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Score movement from Q1 to Q2 (rank change shown in labels)",
    x = NULL,
    y = "Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())
