## ----setup, include=FALSE------------------------------------------------
library(knitr)
options(digits=3)
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(oilabs)

## ----makeus12, echo = FALSE----------------------------------------------
us12 <- 
  atheism %>% 
  filter(nationality == "United States" & year == "2012")

## ----construct-----------------------------------------------------------
n <- length(us12$response)
p_hat <- 50/1002
n * p_hat
SE <- sqrt((p_hat * (1 - p_hat)) / n)
z_star <- qnorm(.025) # for a 95% CI
z_star
MoE <- z_star * SE
c(p_hat - MoE, p_hat + MoE)

## ----ciforus, fig.height=2-----------------------------------------------
inference(y = us12$response, est = "proportion", type = "ci", 
          method = "theoretical", success = "atheist")

## ----ecuador-------------------------------------------------------------
p <- 0.02
n <- 400
p_hats <- rep(0, 10000)

for (i in 1:10000) {
  samp <- sample(c("atheist", "non_atheist"), size = n,
                 replace = TRUE, prob = c(p, 1 - p))
  p_hats[i] <- sum(samp == "atheist") / n
}

## ----ecuadorplot, warning = FALSE, message = FALSE, echo = FALSE---------
qplot(x = p_hats, geom = "density", adjust = 1.7, ylim = c(0, 60))

## ----ecuadorplot2, warning = FALSE, message = FALSE, echo = FALSE--------
qplot(x = p_hats, geom = "density", adjust = 1.7, ylim = c(0, 60)) +
  stat_function(fun = dnorm, args = c(mean = p, sd = sqrt((p*(1-p))/n)), col = "tomato")

## ----ecuador2------------------------------------------------------------
p <- 0.02
n <- 800
p_hats <- rep(0, 10000)

for (i in 1:10000) {
  samp <- sample(c("atheist", "non_atheist"), size = n,
                 replace = TRUE, prob = c(p, 1 - p))
  p_hats[i] <- sum(samp == "atheist") / n
}

## ----ecuadorplot3, warning = FALSE, message = FALSE, echo = FALSE--------
qplot(x = p_hats, geom = "density", adjust = 1.7) +
  stat_function(fun = dnorm, args = c(mean = p, sd = sqrt((p*(1-p))/n)), col = "tomato")

## ----ecuador3------------------------------------------------------------
p <- 0.25
n <- 800
p_hats <- rep(0, 10000)

for (i in 1:10000) {
  samp <- sample(c("atheist", "non_atheist"), size = n,
                 replace = TRUE, prob = c(p, 1 - p))
  p_hats[i] <- sum(samp == "atheist") / n
}

## ----ecuadorplot4, warning = FALSE, message = FALSE, echo = FALSE--------
qplot(x = p_hats, geom = "density", adjust = 1.7) +
  stat_function(fun = dnorm, args = c(mean = p, sd = sqrt((p*(1-p))/n)), col = "tomato")

