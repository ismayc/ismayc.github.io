library(tidyverse)
library(janitor)

players_2022 <- nbastatR::seasons_rosters(seasons = 2022)

players_2022_clean <- clean_names(players_2022)

mean_jersey <- mean(players_2022_clean$number_jersey, na.rm = TRUE)
median_jersey <- median(players_2022_clean$number_jersey, na.rm = TRUE)

mean_height <- mean(players_2022_clean$height_inches, na.rm = TRUE)
median_height <- median(players_2022_clean$height_inches, na.rm = TRUE)

ggplot(data = players_2022_clean, aes(x = number_jersey)) +
  geom_histogram(color = "white")

# 1998

players_1998 <- nbastatR::seasons_rosters(seasons = 1998)

players_1998_clean <- clean_names(players_1998)

mean_jersey <- mean(players_1998_clean$number_jersey, na.rm = TRUE)
median_jersey <- median(players_1998_clean$number_jersey, na.rm = TRUE)

mean_height <- mean(players_1998_clean$height_inches, na.rm = TRUE)
median_height <- median(players_1998_clean$height_inches, na.rm = TRUE)

ggplot(data = players_1998_clean, aes(x = number_jersey)) +
  geom_histogram(color = "white")

# 1992
players_1992 <- nbastatR::seasons_rosters(seasons = 1992)

players_1992_clean <- clean_names(players_1992)

mean_jersey_ <- mean(players_1992_clean$number_jersey, na.rm = TRUE)
median_jersey <- median(players_1992_clean$number_jersey, na.rm = TRUE)

mean_height <- mean(players_1992_clean$height_inches, na.rm = TRUE)
median_height <- median(players_1992_clean$height_inches, na.rm = TRUE)

ggplot(data = players_1992_clean, aes(x = number_jersey)) +
  geom_histogram(color = "white")

# 1972
players_1972 <- nbastatR::seasons_rosters(seasons = 1972)

players_1972_clean <- clean_names(players_1972)

mean_jersey <- mean(players_1972_clean$number_jersey, na.rm = TRUE)
median_jersey <- median(players_1972_clean$number_jersey, na.rm = TRUE)

mean_height <- mean(players_1972_clean$height_inches, na.rm = TRUE)
median_height <- median(players_1972_clean$height_inches, na.rm = TRUE)

ggplot(data = players_1972_clean, aes(x = number_jersey)) +
  geom_histogram(color = "white")
