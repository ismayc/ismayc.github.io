library(dplyr)
library(readr)
offshore <- read.delim("offshore_drilling.txt") %>%
  mutate(response = ifelse(position == "do not know", "no opinion", "opinion")) %>%
  select(-position)
write_csv(offshore, "offshore.csv")
