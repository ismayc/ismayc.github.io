library(pdftools)
library(tidyverse)
library(janitor)
library(readxl)

# Rename .Rproj to correct year
# Open .Rproj

year <- "2024-25"

text_from_pdf <- pdf_text(
  pdf = paste0("Over-Under Picks ", year, ".pdf")
)  |>  
  str_split("\n") |> 
  as_tibble(.name_repair = make.names) |> 
  row_to_names(row_number = 1) |> 
  slice(-16, -32) |> 
  slice(-31)

names(text_from_pdf) <- str_replace(names(text_from_pdf), "PROJ WINS", "PROJ_WINS")

cleaned_data <- text_from_pdf %>%
  mutate(data = str_squish(`TEAM   Adonis   Andy    Chester   Jake    Mary    Mike     Phil   Ryan    PROJ_WINS`)) %>%
  separate(data, into = c("TEAM", "Adonis", "Andy", "Chester", "Jake", "Mary", "Mike", "Phil", "Ryan", "PROJ_WINS"), 
           sep = "\\s+", extra = "merge") |> 
  select(-1)

expanded_data <- cleaned_data %>%
  # For each player, separate the score and over/under status
  separate(Adonis, into = c("Adonis_pt", "Adonis_ou"), sep = "(?<=\\d)(?=\\()") %>%
  separate(Andy,   into = c("Andy_pt", "Andy_ou"),   sep = "(?<=\\d)(?=\\()") %>%
  separate(Chester, into = c("Chester_pt", "Chester_ou"), sep = "(?<=\\d)(?=\\()") %>%
  separate(Jake,    into = c("Jake_pt", "Jake_ou"),   sep = "(?<=\\d)(?=\\()") %>%
  separate(Mary,    into = c("Mary_pt", "Mary_ou"),   sep = "(?<=\\d)(?=\\()") %>%
  separate(Mike,    into = c("Mike_pt", "Mike_ou"),   sep = "(?<=\\d)(?=\\()") %>%
  separate(Phil,    into = c("Phil_pt", "Phil_ou"),   sep = "(?<=\\d)(?=\\()") %>%
  separate(Ryan,    into = c("Ryan_pt", "Ryan_ou"),   sep = "(?<=\\d)(?=\\()") %>%
  # Remove parentheses from the over/under columns
  mutate(across(ends_with("_ou"), ~str_remove_all(., "[()]")))

meta <- read_excel("picks.xlsx", sheet = "meta")

# Assuming meta and expanded_data are your two tibbles

# First, pivot expanded_data into long format
long_data <- expanded_data %>%
  # Combine player point and ou columns
  pivot_longer(
    cols = starts_with("Adonis_pt"):starts_with("Ryan_ou"),
    names_to = c("player", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  # Convert over/under column to 'UNDER' or 'OVER'
  mutate(ou = case_when(
    ou == "u" ~ "UNDER",
    ou == "o" ~ "OVER"
  )) %>%
  # Rename the columns for better clarity
  rename(
    wage = pt,
    choice = ou
  ) %>%
  # Join with the meta tibble to get full team names
  left_join(meta, by = c("TEAM" = "abbreviation"))




### THE OLD WAY I DID IT BEFORE CHATGPT :) ####################################

#write.table(text_from_pdf, "picks.txt", row.names = FALSE)

# 1. Change 'PROJ WINS' to 'PROJ_WINS" in picks.txt manually

# 2. Add _pt for the column name after the name and
# _ou for the over/under pick
# For example, From TEAM Adonis Andy Chester...PROJ_WINS
# To "TEAM   Adonis_pt Adonis_ou   Andy_pt Andy_ou     Chester_pt Chester_ou   
# PROJ_WINS"

# 3. Remove any white space from front of short team name in picks.txt manually

# 4. Delete the last line if it is just ""

# DO NOT RE-RUN THE CODE ABOVE. IT WILL CLEAR OUT THE MANUAL STUFF YOU JUST DID!

# meta <- read_excel("picks.xlsx", sheet = "meta")
# 
# picks_from_pdf <- read_table("picks.txt") |> 
#   pivot_longer(cols = ends_with("ou"),
#                names_to = "player",
#                values_to = "choice") |> 
#   pivot_longer(cols = ends_with("pt"),
#                names_to = "player_pt",
#                values_to = "wage") |> 
#   mutate(player = str_replace_all(player, "_ou", ""),
#          player_pt = str_replace_all(player_pt, "_pt", "")) |> 
#   filter(player == player_pt) |> 
#   select(-player_pt) |> 
#   mutate(choice = if_else(choice == "(u)", "UNDER", "OVER")) |> 
#   rename(team_abbr = `"TEAM`,
#          PROJ_WINS = `PROJ_WINS"`) |>
#   mutate(PROJ_WINS = str_replace_all(PROJ_WINS, '"', "")) |> 
#   mutate(team_abbr = str_replace_all(team_abbr, '"', "")) |> 
#   mutate(PROJ_WINS = as.numeric(PROJ_WINS)) |> 
#   inner_join(meta |> select(team, abbreviation),
#              by = c("team_abbr" = "abbreviation")) 
# 
# picks <- picks_from_pdf |> 
#   select(team, player, wage, choice)
# projections <- picks_from_pdf |>
#   inner_join(meta, by = "team") |>
#   select(team, conference, win_projection = PROJ_WINS) |>
#   distinct(team, conference, win_projection)

picks <- long_data |> 
   select(team, player, wage, choice)

projections <- long_data |>
  inner_join(meta, by = c("team", "conference", "division")) |>
  select(team, conference, win_projection = PROJ_WINS) |>
  distinct(team, conference, win_projection)

write_csv(picks, "picks.csv")
write_csv(projections, "projections.csv")
setdiff(x = sort(unique(meta$team)), y = sort(unique(picks$team)))

# Copy the CSV files into the appropriate tabs in picks.xlsx

# Go to 01-picks-formatting.R
