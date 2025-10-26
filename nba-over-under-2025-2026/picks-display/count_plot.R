library(readr)
library(dplyr)
library(plotly)

# Read the data
picks <- readr::read_csv("data/picks.csv", show_col_types = FALSE)
picks$choice <- toupper(picks$choice)

# Calculate count of OVER picks per team
consensus <- picks %>%
  group_by(team) %>%
  summarise(
    over_count = sum(choice == "OVER"),
    total = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(over_count), desc(total - over_count))  # order: most OVER to most UNDER

# Horizontal bar chart: teams on Y, count on X
plot_ly(
  consensus,
  x = ~over_count,
  y = ~reorder(team, over_count),
  type = "bar",
  orientation = "h",
  name = "Over count"
) %>%
  layout(
    xaxis = list(title = "Number of OVER picks"),
    yaxis = list(title = ""),
    showlegend = FALSE
  )
