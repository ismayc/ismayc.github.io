library(readr)
library(dplyr)
library(tidyr)
library(plotly)

# Load data
picks <- readr::read_csv("data/picks.csv", show_col_types = FALSE) %>%
  mutate(choice = toupper(choice))

# Team x Player matrix of direction: +1 = OVER, -1 = UNDER
dir_wide <- picks %>%
  mutate(dir = ifelse(choice == "OVER", 1L, -1L)) %>%
  select(team, player, dir) %>%
  pivot_wider(names_from = player, values_from = dir, values_fill = 0) %>%
  arrange(team)

D <- as.matrix(dir_wide[, -1, drop = FALSE])
players <- colnames(D)
nP <- length(players)

# Pairwise count of same-direction picks
agree_count_mat <- matrix(0L, nP, nP, dimnames = list(players, players))
for (i in seq_len(nP)) {
  for (j in seq_len(nP)) {
    both <- (D[, i] != 0 & D[, j] != 0)
    agree_count_mat[i, j] <- sum(D[both, i] == D[both, j])
  }
}

# Order players to make clusters clearer using simple dissimilarity on agreement rate
max_common <- max(agree_count_mat)  # rough scale for distance
agree_rate <- agree_count_mat / apply(D != 0, 2, function(x) sum(x))  # normalize by teams per column
agree_rate[is.na(agree_rate)] <- 0
dist_mat <- as.dist(1 - (agree_rate + t(agree_rate)) / 2)
ord <- hclust(dist_mat, method = "average")$labels[ord <- ord <- order.dendrogram(as.dendrogram(hclust(dist_mat)))]

A <- agree_count_mat[ord, ord]

# Heatmap with counts in cells, cool-warm colors
plot_ly(
  x = colnames(A),
  y = rownames(A),
  z = A,
  type = "heatmap",
  colorscale = "RdBu",
#  reversescale = TRUE,
  showscale = TRUE,
  text = A,
  texttemplate = "%{text}",
  hovertemplate = "Player i: %{y}<br>Player j: %{x}<br>Same direction: %{z}<extra></extra>"
) %>%
  layout(
    title = "Same-direction picks per player pair",
    xaxis = list(title = "Player"),
    yaxis = list(title = "Player")
  )
