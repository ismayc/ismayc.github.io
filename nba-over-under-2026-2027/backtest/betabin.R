# Same data as prior_weight.R; add strength uncertainty with a beta-binomial.
# The team's rate is Beta(w + k*q, l + k*(1-q)) scaled by c (c < 1 widens it), q = line/82.
src <- readLines("prior_weight.R"); eval(parse(text = src[1:grep("^chk <-", src) + 1]))
suppressMessages(library(purrr))
p_over_bb <- function(d, k, c) {
  q <- d$line / 82
  a <- c * (d$w + k * q); b <- c * (d$gp - d$w + k * (1 - q))
  need <- floor(d$line) + 1 - d$w
  map_dbl(seq_len(nrow(d)), function(i) {
    n <- d$rem[i]; if (need[i] <= 0) return(1); if (need[i] > n) return(0)
    x <- need[i]:n
    sum(exp(lchoose(n, x) + lbeta(x + a[i], n - x + b[i]) - lbeta(a[i], b[i])))
  })
}
grid <- expand_grid(k = c(10, 20, 30, 40, 60), c = c(1, 0.5, 0.3, 0.2, 0.1, 0.05))
res <- pmap_dfr(grid, function(k, c) {
  p <- p_over_bb(chk, k, c)
  tibble(k, c, brier = mean((p - chk$over)^2),
         logloss = -mean(log(pmax(pmin(ifelse(chk$over, p, 1 - p), 1 - 1e-6), 1e-6))))
})
cat("Beta-binomial grid, best 8 by Brier (binomial k=20 was 0.1281, line-only 0.1232):\n")
print(res |> arrange(brier) |> head(8) |> as.data.frame(), digits = 4)
best <- res |> slice_min(brier, n = 1)
cat("\nStage check at best and at k=20 with best c:\n")
st <- chk |> mutate(stage = cut(gp, c(0, 20, 45, 75), labels = c("5-20", "25-45", "50-75")))
for (kk in unique(c(best$k, 20))) {
  st$p <- p_over_bb(st, kk, best$c)
  print(st |> group_by(stage) |> summarise(k = kk, c = best$c, brier = mean((p - over)^2)) |> as.data.frame(), digits = 4)
}
st$p <- p_over_bb(st, best$k, best$c)
cat("\nCalibration at best:\n")
print(st |> mutate(bin = cut(p, c(0, .05, .2, .4, .6, .8, .95, 1), include.lowest = TRUE)) |>
  group_by(bin) |> summarise(n = n(), predicted = mean(p), actual = mean(over)) |> as.data.frame(), digits = 3)
cat("\nLeave one season out (pick k, c on four, score on fifth):\n")
loso <- map_dfr(unique(chk$season), function(s) {
  tr <- filter(chk, season != s); te <- filter(chk, season == s)
  r <- pmap_dfr(grid, \(k, c) tibble(k, c, b = mean((p_over_bb(tr, k, c) - tr$over)^2))) |> slice_min(b, n = 1)
  tibble(held_out = s, k = r$k, c = r$c,
         brier_bb = mean((p_over_bb(te, r$k, r$c) - te$over)^2),
         brier_binom_k20 = mean((p_over_bb(te, 20, 1e6) - te$over)^2))
})
print(as.data.frame(loso), digits = 4)
