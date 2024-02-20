library(pdftools)
library(tidyverse)
library(janitor)
library(readxl)

text_from_pdf <- pdf_text(pdf = "Over-Under Picks 2022-23.pdf") %>% 
  str_split("\n") %>% 
  as_tibble(.name_repair = make.names) %>% 
  row_to_names(row_number = 1) %>% 
  slice(-16, -32)

write.table(text_from_pdf, "picks.txt", row.names = FALSE)

# 1. Change 'PROJ WINS' to 'PROJ_WINS" in picks_test.txt manually

# 2. Add _pt for the column name after the name and
# _ou for the over/under pick
# For example, From TEAM Adonis Andy Chester...PROJ_WINS
# To "TEAM   Adonis_pt Adonis_ou   Andy_pt Andy_ou     Chester_pt Chester_ou   
# PROJ_WINS"

# 3. Remove any white space from front of short team name

meta <- read_excel("picks.xlsx", sheet = "meta")

picks_from_pdf <- read_table("picks_test.txt") %>% 
  pivot_longer(cols = ends_with("ou"),
               names_to = "player",
               values_to = "choice") %>% 
  pivot_longer(cols = ends_with("pt"),
               names_to = "player_pt",
               values_to = "wage") %>% 
  mutate(player = str_replace_all(player, "_ou", ""),
         player_pt = str_replace_all(player_pt, "_pt", "")) %>% 
  filter(player == player_pt) %>% 
  select(-player_pt) %>% 
  mutate(choice = if_else(choice == "(u)", "UNDER", "OVER")) %>% 
  rename(team_abbr = `"TEAM`,
         PROJ_WINS = `PROJ_WINS"`) %>%
  mutate(PROJ_WINS = str_replace_all(PROJ_WINS, '"', "")) %>% 
  mutate(team_abbr = str_replace_all(team_abbr, '"', "")) %>% 
  mutate(PROJ_WINS = as.numeric(PROJ_WINS)) %>% 
  inner_join(meta %>% select(team, abbreviation),
             by = c("team_abbr" = "abbreviation")) 

picks <- picks_from_pdf %>% 
  select(team, player, wage, choice)
projections <- picks_from_pdf %>% 
  inner_join(meta, by = "team") %>% 
  select(team, conference, win_projection = PROJ_WINS) %>% 
  distinct(team, conference, win_projection)

write_csv(picks, "picks.csv")
write_csv(projections, "projections.csv")
#setdiff(x = picks, y = picks_from_pdf)

# Copy the CSV files into the appropriate tabs in picks.xlsx

# Go to 01-picks-formatting.R
