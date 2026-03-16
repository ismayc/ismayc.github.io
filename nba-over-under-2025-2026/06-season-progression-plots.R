# =============================================================================
# 06-season-progression-plots.R
# Seven visualizations showing how the season has unfolded by player.
# Source AFTER 01-picks-formatting.R, 02b/02c, and 03-data-wrangling.R
# so that projected_score, player_projections_by_team, picks,
# standings_grid, with_picks, standings, projections, out_table,
# scores_tidy, current_rankings, num_games, and nba_season_start_date
# are all available in the environment.
# =============================================================================

library(tidyverse)
library(ggrepel)

# Re-read standings from the latest RDS to guard against stale data
standings <- read_rds(
  here::here("rds", glue::glue("standings_through_{Sys.Date() - 1}.rds"))
)

# ---- shared helpers ---------------------------------------------------------

# Consistent player color palette (8 qualitative colors)
player_colors <- c(
  "Jake"    = "#1b9e77",
  "Andy"    = "#d95f02",
  "Chester" = "#7570b3",
  "Mike"    = "#e7298a",
  "Ryan"    = "#66a61e",
  "Mary"    = "#e6ab02",
  "Phil"    = "#a6761d",
  "Adonis"  = "#666666"
)

# Latest date with data
latest_date <- max(projected_score$Date)

# =============================================================================
# 1. BUMP CHART -- daily rank since Q2 update
# =============================================================================

q2_date <- as.Date("2026-01-15")

daily_ranks <- projected_score %>%
  filter(Date >= q2_date) %>%
  group_by(Date) %>%
  mutate(Rank = min_rank(desc(`Projected Total Points`))) %>%
  ungroup()

# Labels at the right edge
end_labels <- daily_ranks %>%
  filter(Date == latest_date)

# Labels at the left edge (Q2 starting position)
start_labels <- daily_ranks %>%
  filter(Date == min(Date))

ggplot(daily_ranks, aes(x = Date, y = Rank, color = Player, group = Player)) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(
    data = end_labels,
    size = 3
  ) +
  geom_point(
    data = start_labels,
    size = 2,
    alpha = 0.5
  ) +
  geom_text_repel(
    data = end_labels,
    aes(label = paste0(Player, " (", `Projected Total Points`, ")")),
    nudge_x = 2,
    hjust = 0,
    direction = "y",
    size = 3.5,
    segment.alpha = 0.3,
    seed = 42
  ) +
  scale_y_reverse(
    breaks = 1:8,
    labels = paste0("#", 1:8)
  ) +
  scale_color_manual(values = player_colors, guide = "none") +
  scale_x_date(expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title = "Daily leaderboard rank since Q2 update",
    subtitle = "Lower is better. Score shown at current position.",
    x = NULL,
    y = "Rank"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


# =============================================================================
# 2. CUMULATIVE POINTS AREA CHART -- running total by player
# =============================================================================

# with_picks has daily current_projected_points per player per team.
# Sum across teams for each player-date to get daily total.
daily_totals <- with_picks %>%
  group_by(date, player) %>%
  summarize(
    daily_total = sum(current_projected_points, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Date = date, Player = player)

# Order players by current ranking (best on top)
daily_totals <- daily_totals %>%
  mutate(Player = factor(Player, levels = current_rankings))

ggplot(daily_totals, aes(x = Date, y = daily_total, color = Player)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~ Player, nrow = 2) +
  scale_color_manual(values = player_colors, guide = "none") +
  labs(
    title = "Projected point total over the season (by player)",
    subtitle = "Each bump or dip corresponds to a team flipping OVER/UNDER status",
    x = NULL,
    y = "Projected Total Points"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )


# =============================================================================
# 3. PLAYER-BY-TEAM HEATMAP -- who's carrying whom
# =============================================================================

# Get the most recent projected points per player per team
latest_player_team <- player_projections_by_team %>%
  filter(Date == latest_date) %>%
  select(Player, Team, `Current Projected Points`, `Over/Under Pick`)

# Order teams by how divisive they are (high variance across players)
team_divisiveness <- latest_player_team %>%
  group_by(Team) %>%
  summarize(var_pts = var(`Current Projected Points`, na.rm = TRUE)) %>%
  arrange(desc(var_pts))

# Order players by current total
player_totals <- latest_player_team %>%
  group_by(Player) %>%
  summarize(total = sum(`Current Projected Points`, na.rm = TRUE)) %>%
  arrange(desc(total))

heatmap_df <- latest_player_team %>%
  mutate(
    Team = factor(Team, levels = team_divisiveness$Team),
    Player = factor(Player, levels = player_totals$Player)
  )

ggplot(heatmap_df, aes(x = Team, y = Player, fill = `Current Projected Points`)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(
    aes(label = `Current Projected Points`),
    size = 2.5,
    color = "grey20"
  ) +
  scale_fill_gradient2(
    low = "#c51b7d", mid = "white", high = "#4d9221",
    midpoint = 0,
    name = "Points"
  ) +
  labs(
    title = "Current projected points by player and team",
    subtitle = "Teams sorted by divisiveness (most variable on left). Players sorted by total score.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 55, hjust = 1, size = 7),
    panel.grid = element_blank()
  )


# =============================================================================
# 4. AGREEMENT MATRIX -- how similar are each pair of players' picks?
# =============================================================================

# Build a wide matrix of pick directions
pick_directions <- picks %>%
  select(team, player, choice) %>%
  pivot_wider(names_from = player, values_from = choice)

players <- sort(unique(picks$player))

# Compute pairwise agreement (out of 30 teams)
agreement_mat <- expand_grid(p1 = players, p2 = players) %>%
  rowwise() %>%
  mutate(
    agree = sum(pick_directions[[p1]] == pick_directions[[p2]], na.rm = TRUE)
  ) %>%
  ungroup()

# Also compute pairwise score correlation from daily totals
score_wide <- daily_totals %>%
  filter(Date == latest_date) %>%
  select(Player, daily_total) %>%
  deframe()

agreement_mat <- agreement_mat %>%
  mutate(
    score_diff = abs(score_wide[p1] - score_wide[p2])
  )

# Order players by current ranking
agreement_mat <- agreement_mat %>%
  mutate(
    p1 = factor(p1, levels = current_rankings),
    p2 = factor(p2, levels = current_rankings)
  )

ggplot(agreement_mat, aes(x = p1, y = p2, fill = agree)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = agree), size = 4, fontface = "bold") +
  scale_fill_gradient(
    low = "#fee8c8", high = "#e34a33",
    name = "Teams\nagreed on\n(of 30)"
  ) +
  labs(
    title = "Pick agreement between players",
    subtitle = "Number of teams where both players picked the same direction (OVER/UNDER)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# =============================================================================
# 5. SWING TEAMS DOT PLOT -- teams that have flipped OVER/UNDER
# =============================================================================

# Build a definitive list of clinched teams (used in plots 5 and 7)
# Same logic as the main table arrows: wins > projection or
# losses > (82 - projection)
clinched_teams <- projections %>%
  mutate(loss_projection = num_games - win_projection) %>%
  inner_join(
    standings %>% select(team_name, wins, losses),
    by = c("team" = "team_name")
  ) %>%
  mutate(wins = as.numeric(wins), losses = as.numeric(losses)) %>%
  filter(wins > win_projection | losses > loss_projection) %>%
  pull(team)

message("Clinched teams (", length(clinched_teams), "): ",
        paste(sort(clinched_teams), collapse = ", "))

# Identify teams that have changed over/under status at some point
# but have NOT yet clinched (still in play)
team_flips <- standings_grid %>%
  filter(!is.na(over_under)) %>%
  group_by(team) %>%
  summarize(
    n_statuses = n_distinct(over_under),
    .groups = "drop"
  ) %>%
  filter(n_statuses > 1) %>%
  filter(!(team %in% clinched_teams)) %>%
  pull(team)

# For each swing team, show each player's wager and whether they're currently right
swing_data <- player_projections_by_team %>%
  filter(Date == latest_date, Team %in% team_flips) %>%
  select(Player, Team, `Current Projected Points`, `Over/Under Pick`,
         `Current Over/Under`) %>%
  mutate(
    currently_correct = `Over/Under Pick` == `Current Over/Under`,
    direction_label = if_else(
      `Over/Under Pick` == "OVER", "\u2191", "\u2193"
    )
  )

# Get wage from picks
swing_data <- swing_data %>%
  inner_join(
    picks %>% select(team, player, wage),
    by = c("Team" = "team", "Player" = "player")
  )

if (nrow(swing_data) > 0) {
  
  ggplot(
    swing_data,
    aes(
      x = wage,
      y = reorder(Player, `Current Projected Points`),
      color = currently_correct,
      shape = `Over/Under Pick`
    )
  ) +
    geom_point(size = 3.5) +
    geom_text(
      aes(label = direction_label),
      nudge_x = 0.6,
      size = 3.5,
      show.legend = FALSE
    ) +
    facet_wrap(~ Team, scales = "free_y") +
    scale_color_manual(
      values = c("TRUE" = "forestgreen", "FALSE" = "firebrick"),
      labels = c("TRUE" = "Correct", "FALSE" = "Wrong"),
      name = "Currently"
    ) +
    scale_shape_manual(
      values = c("OVER" = 17, "UNDER" = 25),
      name = "Pick"
    ) +
    labs(
      title = "Swing teams: bets that could still go either way",
      subtitle = "Teams that have flipped OVER/UNDER at least once and have NOT yet clinched",
      x = "Wager",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )
  
} else {
  message("All swing teams have since clinched -- no swing teams to show.")
}


# =============================================================================
# 6. PACE-OF-CLINCH TIMELINE -- when outcomes got locked in
# =============================================================================

# For each team, find the first date where Outcome Determined != "not yet"
clinch_dates <- player_projections_by_team %>%
  filter(`Outcome Determined` != "not yet") %>%
  arrange(Date) %>%
  group_by(Team) %>%
  summarize(
    clinch_date = min(Date),
    outcome = first(`Outcome Determined`),
    .groups = "drop"
  ) %>%
  arrange(clinch_date)

if (nrow(clinch_dates) > 0) {
  
  # For each clinched team, compute each player's point impact
  clinch_impact <- clinch_dates %>%
    inner_join(
      picks %>% select(team, player, wage, choice),
      by = c("Team" = "team")
    ) %>%
    mutate(
      points = if_else(
        choice == as.character(outcome),
        wage, -wage
      ),
      correct = choice == as.character(outcome)
    )
  
  # Net impact per player per clinch event
  clinch_player_net <- clinch_impact %>%
    group_by(clinch_date, player) %>%
    summarize(
      net_points = sum(points),
      teams_clinched = n(),
      .groups = "drop"
    )
  
  # Timeline plot: each clinch event as a point, colored by outcome
  clinch_timeline <- clinch_dates %>%
    mutate(
      label = paste0(Team, "  (", format(clinch_date, "%b %d"), ")"),
      y_pos = row_number()
    )
  
  ggplot(clinch_timeline, aes(x = clinch_date, y = y_pos)) +
    geom_segment(
      aes(xend = clinch_date, y = 0, yend = y_pos),
      color = "grey80",
      linewidth = 0.4
    ) +
    geom_point(
      aes(color = outcome),
      size = 3
    ) +
    geom_text_repel(
      aes(label = label, color = outcome),
      hjust = 0,
      nudge_x = 1,
      direction = "y",
      size = 3.2,
      segment.color = "grey70",
      segment.size = 0.3,
      segment.alpha = 0.5,
      min.segment.length = 0,
      box.padding = 0.2,
      point.padding = 0.3,
      force = 1.5,
      max.overlaps = Inf,
      seed = 42,
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = c("OVER" = "forestgreen", "UNDER" = "deeppink"),
      name = "Clinched"
    ) +
    scale_x_date(
      expand = expansion(mult = c(0.02, 0.25))
    ) +
    labs(
      title = "When did each team's outcome get locked in?",
      subtitle = "Timeline of OVER/UNDER clinch dates through the season",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Supplementary: who benefited most from clinches?
  
  # Give each clinch event an ordering, with a fractional day offset
  # for same-date events so the steps are visually separated
  clinch_dates_offset <- clinch_dates %>%
    arrange(clinch_date) %>%
    group_by(clinch_date) %>%
    mutate(
      date_seq = row_number() - 1,
      plot_date = clinch_date + date_seq * 0.5
    ) %>%
    ungroup()
  
  clinch_impact_offset <- clinch_dates_offset %>%
    select(Team, clinch_date, plot_date) %>%
    inner_join(
      clinch_impact %>% select(-clinch_date),
      by = "Team"
    )
  
  # Add an origin row per player (zero points, just before the first event)
  first_plot_date <- min(clinch_dates_offset$plot_date) - 1
  origin_rows <- clinch_impact_offset %>%
    distinct(player) %>%
    mutate(
      Team = NA_character_,
      clinch_date = first_plot_date,
      plot_date = first_plot_date,
      outcome = NA,
      points = 0,
      correct = NA,
      cum_clinch_points = 0
    )
  
  clinch_cumulative <- clinch_impact_offset %>%
    arrange(plot_date) %>%
    group_by(player) %>%
    mutate(cum_clinch_points = cumsum(points)) %>%
    ungroup() %>%
    bind_rows(origin_rows) %>%
    arrange(player, plot_date)
  
  # Fixed jitter per player applied to BOTH lines and dots
  player_offsets <- tibble(
    player = names(player_colors),
    y_jitter = seq(-1.2, 1.2, length.out = length(player_colors)),
    x_jitter = seq(-0.4, 0.4, length.out = length(player_colors))
  )
  
  clinch_cumulative <- clinch_cumulative %>%
    inner_join(player_offsets, by = "player") %>%
    mutate(
      cum_jittered = cum_clinch_points + y_jitter,
      plot_date_jittered = plot_date + x_jitter
    )
  
  # Label each clinch event -- collapse same-date teams into one label
  clinch_event_labels <- clinch_dates %>%
    group_by(clinch_date) %>%
    summarize(
      label = paste0(
        paste(Team, collapse = "\n"),
        "\n(", format(first(clinch_date), "%b %d"), ")"
      ),
      .groups = "drop"
    )
  
  # Compute a y position for labels: just above the max data value
  y_max <- max(clinch_cumulative$cum_jittered, na.rm = TRUE)
  y_min <- min(clinch_cumulative$cum_jittered, na.rm = TRUE)
  label_y <- y_max + (y_max - y_min) * 0.05
  
  ggplot(clinch_cumulative) +
    # Both lines and dots use the same jittered x and y
    geom_step(
      aes(x = plot_date_jittered, y = cum_jittered,
          color = player, group = player),
      linewidth = 0.7, alpha = 0.7
    ) +
    geom_point(
      data = clinch_cumulative %>% filter(!is.na(Team)),
      aes(x = plot_date_jittered, y = cum_jittered,
          color = player),
      size = 1.8, alpha = 0.8
    ) +
    # Vertical reference lines at each original clinch date
    geom_vline(
      data = clinch_event_labels,
      aes(xintercept = clinch_date),
      linetype = "dotted",
      color = "grey75",
      linewidth = 0.3
    ) +
    # Team name + date labels along the top (inside plot area)
    geom_label_repel(
      data = clinch_event_labels,
      aes(x = clinch_date, y = label_y, label = label),
      inherit.aes = FALSE,
      size = 2.5,
      color = "grey30",
      fill = "white",
      alpha = 0.85,
      label.padding = unit(0.15, "lines"),
      label.r = unit(0.15, "lines"),
      direction = "x",
      nudge_y = 5,
      segment.color = "grey60",
      segment.size = 0.3,
      segment.alpha = 0.5,
      min.segment.length = 0,
      box.padding = 0.3,
      force = 2,
      max.overlaps = Inf,
      seed = 42
    ) +
    scale_color_manual(values = player_colors, name = "Player") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    coord_cartesian(clip = "off") +
    labs(
      title = "Cumulative points from clinched outcomes",
      subtitle = "Each step corresponds to a team locking in its OVER or UNDER result",
      x = NULL,
      y = "Cumulative Points from Clinches"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
    )
  
} else {
  message("No teams have clinched yet -- skipping clinch timeline plots.")
}


# =============================================================================
# 7. WHAT-IF SENSITIVITY BARS -- point swings if undetermined teams flip
# =============================================================================

# Teams whose outcome is not yet determined
# Use clinched_teams (built before plot 5) for a robust exclusion
undetermined <- setdiff(unique(picks$team), clinched_teams)

# Further filter to teams where the outcome is genuinely uncertain.
# Exclude teams that are effectively locked in even if not mathematically
# clinched (e.g., needing 1 win in 19 games, or needing to go undefeated).
# Keep only teams where the required win% in remaining games is between
# 15% and 85%.
contested_teams <- out_table %>%
  filter(as.character(`Outcome Determined`) == "not yet") %>%
  separate(
    `Current Record`,
    into = c("curr_wins", "curr_losses"),
    sep = "-",
    remove = FALSE,
    convert = TRUE
  ) %>%
  mutate(
    remaining = num_games - curr_wins - curr_losses,
    wins_needed = ceiling(`Win Projection`) - curr_wins,
    win_pct_needed = wins_needed / remaining
  ) %>%
  filter(win_pct_needed >= 0.15, win_pct_needed <= 0.85) %>%
  pull(Team)

message("Contested teams (", length(contested_teams), "): ",
        paste(sort(contested_teams), collapse = ", "))

if (length(contested_teams) > 0) {
  
  # Current over/under status for contested teams
  current_status <- standings_grid %>%
    filter(team %in% contested_teams, date == latest_date) %>%
    select(team, over_under)
  
  # For each contested team, compute point swing per player if it flips
  what_if <- picks %>%
    filter(team %in% contested_teams) %>%
    inner_join(current_status, by = "team") %>%
    mutate(
      # Current points from this team
      current_pts = case_when(
        is.na(over_under) ~ 0,
        over_under == "PUSH" ~ 0,
        choice == over_under ~ wage,
        TRUE ~ -wage
      ),
      # Flipped over_under
      flipped_ou = if_else(over_under == "OVER", "UNDER", "OVER"),
      # Points if flipped
      flipped_pts = case_when(
        choice == flipped_ou ~ wage,
        TRUE ~ -wage
      ),
      # Net swing
      swing = flipped_pts - current_pts
    ) %>%
    filter(swing != 0)
  
  if (nrow(what_if) > 0) {
    # Order teams by total absolute swing (most impactful first)
    team_impact_order <- what_if %>%
      group_by(team) %>%
      summarize(total_abs_swing = sum(abs(swing))) %>%
      arrange(desc(total_abs_swing)) %>%
      pull(team)
    
    what_if <- what_if %>%
      mutate(team = factor(team, levels = team_impact_order))
    
    ggplot(
      what_if,
      aes(
        x = swing,
        y = reorder(player, swing),
        fill = swing > 0
      )
    ) +
      geom_col(width = 0.7) +
      geom_text(
        aes(
          label = if_else(swing > 0, paste0("+", swing), as.character(swing)),
          hjust = if_else(swing > 0, -0.2, 1.2)
        ),
        size = 2.8
      ) +
      geom_vline(xintercept = 0, linewidth = 0.5, color = "grey40") +
      facet_wrap(~ team, scales = "free_y") +
      scale_fill_manual(
        values = c("TRUE" = "forestgreen", "FALSE" = "firebrick"),
        guide = "none"
      ) +
      scale_x_continuous(expand = expansion(mult = c(0.3, 0.3))) +
      labs(
        title = "What if this team flips? Point swing per player",
        subtitle = paste0(
          "Teams where the required win% in remaining games is between 15%-85%\n",
          "Shows how each player's score would change if the team's current OVER/UNDER status reversed"
        ),
        x = "Point Swing",
        y = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank()
      )
    
  } else {
    message("No meaningful swings among undetermined teams.")
  }
  
} else {
  message("No genuinely contested teams remain -- all outcomes are effectively decided!")
}