library(readr)
library(dplyr)
library(tidyr)
library(plotly)

# Read your data (adjust path as needed)
picks <- readr::read_csv("data/picks.csv", show_col_types = FALSE)
picks$choice <- toupper(picks$choice)

# Prepare wide-format data with signed wagers
signed_wide <- picks %>%
  mutate(signed = ifelse(choice == "OVER", wage, -wage)) %>%
  select(team, player, signed) %>%
  tidyr::pivot_wider(names_from = player, values_from = signed, values_fill = 0) %>%
  arrange(desc(team))   # reverse alphabetical order

hm <- as.matrix(signed_wide[, -1, drop = FALSE])
rownames(hm) <- signed_wide$team

# Convert numeric values to "N OVER"/"N UNDER" text
txt <- matrix(
  ifelse(hm > 0, paste0(abs(hm), " OVER"),
         ifelse(hm < 0, paste0(abs(hm), " UNDER"), "")),
  nrow = nrow(hm),
  dimnames = list(rownames(hm), colnames(hm))
)

# Symmetric color range around zero
zr <- max(abs(hm))

plot_ly(
  x = colnames(hm),
  y = rownames(hm),
  z = hm,
  type = "heatmap",
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = -zr,
  zmax = zr,
  zmid = 0,
  showscale = FALSE,     # removes the color scale bar
  text = txt,
  texttemplate = "%{text}",
  textfont = list(size = 10),
  hovertemplate = "Team: %{y}<br>Participant: %{x}<br>Wager: %{text}<extra></extra>"
) %>%
  layout(
    showlegend = FALSE      # removes the legend
#    xaxis = list(title = "Participant"),
#    yaxis = list(title = "Team")
)



