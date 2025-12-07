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
player_order <- c(
  "Mary",
  "Chester",
  "Phil",
  "Mike",
  "Adonis",
  "Ryan",
  "Andy",
  "Jake"
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
    player == "Mary"   ~ "1. Mary",
    player == "Chester" ~ "2. Chester",
    player == "Phil"   ~ "3. Phil",
    player == "Mike"   ~ "4. Mike",
    player == "Adonis" ~ "5. Adonis",
    player == "Ryan"   ~ "6. Ryan",
    player == "Andy"   ~ "7. Andy",
    player == "Jake"   ~ "8. Jake"
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
    title = "Most differentiating teams per player",
    x = "Team",
    y = "Absolute deviation from group",
    fill = "Mismatch with actual"
  ) +
  theme_minimal()

