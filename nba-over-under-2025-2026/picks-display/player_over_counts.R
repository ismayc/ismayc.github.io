library(readr)
library(dplyr)
library(tidyr)
library(plotly)

# Load data
picks <- readr::read_csv("data/picks.csv", show_col_types = FALSE)
picks$choice <- toupper(picks$choice)

# Count number of OVER and UNDER picks per participant
player_counts <- picks %>%
  count(player, choice, name = "n") %>%
  tidyr::pivot_wider(names_from = choice, values_from = n, values_fill = 0) %>%
  arrange(desc(OVER), desc(UNDER))

# Convert back to long format for plotting
plot_data <- player_counts %>%
  tidyr::pivot_longer(c(OVER, UNDER), names_to = "choice", values_to = "n") %>%
  mutate(player = factor(player, levels = player_counts$player))

# Create the main bar plot
p <- plot_ly(
  plot_data,
  x = ~n,
  y = ~player,
  color = ~choice,
  type = "bar",
  orientation = "h"
) %>%
  layout(
    barmode = "group",
    xaxis = list(title = "Number of picks"),
    yaxis = list(title = "Participant"),
    legend = list(orientation = "h")
  )

# Add text labels at the end of the OVER bars
p <- p %>%
  add_text(
    data = subset(plot_data, choice == "OVER"),
    text = ~n,
    textposition = "right",
    textfont = list(size = 20, color = "black"),
    showlegend = FALSE,
    hoverinfo = "none",
    x = ~n,
    y = ~player,
    orientation = "h",
    type = "bar",
    marker = list(color = "rgba(0,0,0,0)") # invisible markers for label placement
  )

p
