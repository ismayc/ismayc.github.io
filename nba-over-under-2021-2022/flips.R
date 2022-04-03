library(tidyr)
library(dplyr)
flips <- expand_grid(
  `Dallas Mavericks` = c("OVER", "UNDER"),
  `Denver Nuggets` = c("OVER", "UNDER"),
  `Detroit Pistons` = c("OVER", "UNDER"),
  `Houston Rockets` = c("OVER", "UNDER"),
  `New Orleans Pelicans` = c("OVER", "UNDER"),
  `Oklahoma City Thunder` = c("OVER", "UNDER"),
  `Orlando Magic` = c("OVER", "UNDER"),
  `Philadelphia 76ers` = c("OVER", "UNDER"),
  `San Antonio Spurs` = c("OVER", "UNDER"),
  `Washington Wizards` = c("OVER", "UNDER")
) %>%
  mutate(outcome = row_number())

long_flips <- flips %>%
  pivot_longer(cols = -outcome, names_to = "team", values_to = "result")

# Determine points where outcomes determined for each player
# Add on points according to each possibility in long_flips
