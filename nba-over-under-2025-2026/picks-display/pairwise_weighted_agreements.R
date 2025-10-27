library(readr)
library(dplyr)
library(tidyr)
library(plotly)
library(purrr)

# Load and prep
picks <- readr::read_csv("data/picks.csv", show_col_types = FALSE) %>%
  mutate(choice = toupper(choice),
         signed = ifelse(choice == "OVER", wage, -wage))

# Team x Player matrix of signed wagers (OVER = +wage, UNDER = -wage, missing = 0)
wide <- picks %>%
  select(team, player, signed) %>%
  pivot_wider(names_from = player, values_from = signed, values_fill = 0) %>%
  arrange(team)

M <- as.matrix(wide[, -1, drop = FALSE])
rownames(M) <- wide$team
players <- colnames(M)
P <- length(players)

# Helper to build pairwise matrices
pairwise_apply <- function(fun) {
  out <- matrix(0, nrow = P, ncol = P, dimnames = list(players, players))
  for (i in seq_len(P)) {
    for (j in seq_len(P)) {
      out[i, j] <- fun(M[, i], M[, j])
    }
  }
  out
}

# 1) Agreement count: same direction on a team where both wagered nonzero
agree_count_fun <- function(a, b) {
  both <- (a != 0 & b != 0)
  sum(sign(a[both]) == sign(b[both]))
}
agree_count_mat <- pairwise_apply(agree_count_fun)

# 2) Weighted agreement: sum over teams of pmin(|a|, |b|) where directions match
weighted_agree_fun <- function(a, b) {
  both <- (a != 0 & b != 0)
  same <- (sign(a) == sign(b)) & both
  sum(pmin(abs(a[same]), abs(b[same])))
}
weighted_agree_mat <- pairwise_apply(weighted_agree_fun)

# 3) Cosine similarity on signed wagers
cosine_fun <- function(a, b) {
  num <- sum(a * b)
  den <- sqrt(sum(a^2)) * sqrt(sum(b^2))
  if (den == 0) return(0)
  num / den
}
cosine_sim_mat <- pairwise_apply(cosine_fun)

# ---- Heatmap of WEIGHTED agreement ----
# Order players by total weighted agreement to make clusters pop
order_idx <- order(rowSums(weighted_agree_mat), decreasing = TRUE)
W <- weighted_agree_mat[order_idx, order_idx, drop = FALSE]

plot_ly(
  x = colnames(W),
  y = rownames(W),
  z = W,
  type = "heatmap",
  colorscale = "RdBu",   # Cool–warm palette (blue to red)
  showscale = TRUE,
  hovertemplate = "Player i: %{y}<br>Player j: %{x}<br>Weighted agreement: %{z}<extra></extra>"
) %>%
  layout(
    title = "Pairwise weighted agreement",
    xaxis = list(title = "Player"),
    yaxis = list(title = "Player")
  )

# ---- 2D map of player closeness from cosine similarity ----
# Turn cosine similarity into an angular distance in [0, 1]
cos_clipped <- pmin(pmax(cosine_sim_mat, -1), 1)
angle <- acos(cos_clipped)           # radians in [0, pi]
dist_mat <- angle / pi               # scale to [0, 1]
# Classical MDS to 2D
fit <- cmdscale(as.dist(dist_mat), k = 2, eig = FALSE)
mds_df <- data.frame(player = rownames(fit), x = fit[, 1], y = fit[, 2])

plot_ly(
  mds_df, x = ~x, y = ~y, text = ~player, type = "scatter", mode = "markers+text",
  textposition = "top center"
) %>%
  layout(
    title = "Players mapped by similarity of signed wagers",
    xaxis = list(title = "Dim 1"),
    yaxis = list(title = "Dim 2"),
    showlegend = FALSE
  )
