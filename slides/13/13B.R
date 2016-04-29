## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(rglwidget)
options(digits=3, width=80)
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", warning = FALSE)
library(rgl)
knit_hooks$set(webgl = hook_webgl)
library(dplyr)
library(ggplot2)
library(oilabs)
library(openintro)
library(plotly)

## ------------------------------------------------------------------------
sock_pairs <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
sock_singles <- c("l", "m", "n", "o", "p")
socks <- c(rep(sock_pairs, each = 2), sock_singles)
socks

## ------------------------------------------------------------------------
picked_socks <- sample(socks, size = 11, replace = FALSE); picked_socks
sock_counts <- table(picked_socks); sock_counts
n_singles <- sum(sock_counts == 1); n_singles

## ----echo = FALSE--------------------------------------------------------
pick_socks <- function(N_pairs, N_singles, N_pick) {
  N_sock_types <- N_pairs + N_singles
  socks <- rep(1:N_sock_types, rep( 2:1, c(N_pairs, N_singles) ))
  picked_socks <- sample(socks, 11)
  sock_counts <- table(picked_socks)
  n_singles <- sum(sock_counts == 1)
  n_singles
}
set.seed(200)

## ------------------------------------------------------------------------
pick_socks(N_pairs = 9, N_singles = 5, N_pick = 11)
pick_socks(9, 5, 11)
pick_socks(9, 5, 11)

## ----echo = FALSE, cache = TRUE------------------------------------------
sim_singles <- rep(0, 1000)

for (i in 1:1000) {
  sim_singles[i] <- pick_socks(9, 5, 11)
}

qplot(as.factor(sim_singles), geom = "bar", xlab = "number of singletons")

## ----echo = FALSE--------------------------------------------------------
qplot(as.factor(sim_singles), geom = "bar", xlab = "number of singletons") +
  geom_vline(xintercept = 6, col = "tomato")

## ------------------------------------------------------------------------
table(sim_singles)
table(sim_singles)[6]/1000

## ----echo = FALSE--------------------------------------------------------
pval <- table(sim_singles)[6]/1000

## ---- cache = TRUE, echo = FALSE, warning=FALSE--------------------------
x <- rnbinom(1e6, mu = 30, size = -30^2 / (30 - 15^2))
(prior_n <- qplot(x, geom = "histogram", xlab = "number of socks", binwidth = 1, xlim = c(0, 100),
      fill = I("darkgreen"), ylab = "prob. density", main = "P(parameter)"))

## ----cache = TRUE, echo = FALSE------------------------------------------
y <- rbeta(1e6, shape1 = 15, shape2 = 2)
(prior_p <- qplot(y, geom = "histogram", xlab = "proportion of pairs", binwidth = .01, xlim = c(0, 1),
      fill = I("darkgreen"), ylab = "prob. density", main = "P(parameter)"))

## ----cache = TRUE, echo = FALSE------------------------------------------
sock_sim <- t(replicate(100000, {
  n_socks <- rnbinom(1, mu = 30, size = -30^2 / (30 - 15^2) )
  prop_pairs <- rbeta(1, shape1 = 15, shape2 = 2)
  n_pairs <- round(floor(n_socks / 2) * prop_pairs)
  n_odd <- n_socks - n_pairs * 2
  n_sock_types <- n_pairs + n_odd
  socks <- rep(seq_len(n_sock_types), rep( 2:1, c(n_pairs, n_odd) ))
  picked_socks <- sample(socks, size =  min(11, n_socks))
  sock_counts <- table(picked_socks)
  c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
    n_socks = n_socks, prop_pairs = prop_pairs)
}))
sock_sim <- as.data.frame(sock_sim)
post_samples <- sock_sim %>%
  filter(unique == 11, pairs == 0)

## ------------------------------------------------------------------------
head(sock_sim, 3)
sock_sim %>%
  filter(unique == 11, pairs == 0) %>%
  head(3)

## ----echo = FALSE--------------------------------------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## ----cache = TRUE, echo = FALSE, fig.width = 8.5, warning=FALSE----------
post_p <- qplot(prop_pairs, data = post_samples, geom = "histogram", 
                xlab = "proportion of pairs", binwidth = .01, xlim = c(0, 1),
                fill = I("darkgreen"), ylab = "prob. density", main = "P(parameter|data)")
multiplot(prior_p, post_p, layout = matrix(1:2, ncol = 2, byrow = TRUE))

## ----cache = TRUE, echo = FALSE, fig.width = 8.5, warning=FALSE----------
post_n <- qplot(n_socks, data = post_samples, geom = "histogram", 
                xlab = "number of socks", binwidth = 1, xlim = c(0, 100),
                fill = I("darkgreen"), ylab = "prob. density", main = "P(parameter|data)")
multiplot(prior_n, post_n, layout = matrix(1:2, ncol = 2, byrow = TRUE))

## ----cache = TRUE, echo = FALSE, fig.height = 3, fig.width = 5, warning=FALSE----
post_n

## ----cache = TRUE, echo = FALSE, fig.height = 3, fig.width = 5-----------
qplot(n_socks, data = post_samples, geom = "histogram", 
                xlab = "number of socks", binwidth = 1, xlim = c(0, 100),
                fill = I("darkgreen"), ylab = "prob. density", main = "P(parameter|data)") +
  geom_vline(xintercept = median(post_samples$n_socks), col = "goldenrod")

