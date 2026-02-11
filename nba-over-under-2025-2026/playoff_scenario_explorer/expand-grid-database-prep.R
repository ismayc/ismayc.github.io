library(tidyverse)
library(arrow)

picks <- readxl::read_excel("picks.xlsx", sheet = "picks")

picks_wide_points <- picks %>%
  select(team:wage) %>%
  pivot_wider(names_from = player,
              values_from = wage)

picks_wide_choice <- picks %>%
  select(team, player, choice) %>%
  pivot_wider(names_from = player,
              values_from = choice)

points_names <- names(picks_wide_points)[names(picks_wide_points) != "team"]
choice_names <- names(picks_wide_choice)[names(picks_wide_choice) != "team"]

points_names <- paste0(points_names, "_points")
choice_names <- paste0(stringr::str_to_lower(choice_names), "_choice")

names(picks_wide_points) <- c("team", points_names)
names(picks_wide_choice) <- c("team", choice_names)

picks_wide_new <- picks_wide_points %>%
  inner_join(picks_wide_choice)

picks_wide_new <- picks_wide_new[, order(names(picks_wide_new))] %>%
  relocate(team, everything())

write_parquet(picks_wide_new, "picks_wide_new.parquet")

readr::write_rds(
  picks_wide_new, "picks_wide_new.rds")