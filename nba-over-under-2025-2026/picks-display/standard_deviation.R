library(readr)
library(dplyr)
library(plotly)

# Read the data
picks <- readr::read_csv("data/picks.csv", show_col_types = FALSE)
picks$choice <- toupper(picks$choice)

# Compute signed wagers: OVER = positive, UNDER = negative
picks <- picks %>%
  mutate(signed_wage = ifelse(choice == "OVER", wage, -wage))


# ---- Standard deviation by team ---- 
team_sd <- picks %>% group_by(team) %>% summarise(sd_signed = sd(signed_wage), .groups = "drop") %>% 
  arrange(desc(team)) # reverse alphabetical order 


plot_data <- team_sd %>% arrange(desc(sd_signed)) %>%
  mutate(team = factor(team, levels = rev(team)))

plot_ly() %>%
  add_segments(
    data = plot_data,
    x = 0, xend = ~sd_signed,
    y = ~team, yend = ~team,
    line = list(color = "lightgray", width = 2)
  ) %>%
  add_markers(
    data = plot_data,
    x = ~sd_signed, y = ~team,
    size = 20,                     # make dots larger
    marker = list(color = "black", line = list(width = 1, color = "black"))
  ) %>%
  layout(
    xaxis = list(title = "Standard deviation of signed wagers"),
    yaxis = list(title = "Team"),
    showlegend = FALSE
  )
