library(pdftools)
library(tidyverse)

write.table(pdf_text(pdf = "Over-Under Picks 2021-22.pdf"), "picks.txt")

picks_from_pdf <- read.table("picks.txt", header = TRUE) %>% 
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
  rename(team_abbr = team) %>% 
  inner_join(meta %>% select(team, abbreviation),
             by = c("team_abbr" = "abbreviation")) %>% 
  select(team, player, wage, choice)

setdiff(x = picks, y = picks_from_pdf)

