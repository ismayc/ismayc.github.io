## ----setup, include=FALSE------------------------------------------------
library(knitr)
options(digits=3)
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(oilabs)

## ----sim-----------------------------------------------------------------
n <- 412
p <- c(.043, .02, .125, .77, .042)
samp <- sample(c("asian", "black", "hispanic", "white", "other"), 
       size = n, replace = TRUE, prob = p)
table(samp)
obs <- c(46, 31, 42, 258, 35)

## ----sim2----------------------------------------------------------------
obs <- c(46, 31, 42, 258, 35)
samp <- sample(c("asian", "black", "hispanic", "white", "other"), 
       size = n, replace = TRUE, prob = p)
table(samp)
samp <- sample(c("asian", "black", "hispanic", "white", "other"), 
       size = n, replace = TRUE, prob = p)
table(samp)

## ----sim3----------------------------------------------------------------
n <- 412
p <- c(.043, .02, .125, .042, .77)
chisqs <- rep(0, 1000)
set.seed(405)

for(i in 1:1000) {
  samp <- sample(c("asian", "black", "hispanic", "other", "white"), 
       size = n, replace = TRUE, prob = p)
  obs <- c(table(samp))
  chisqs[i] <- chisq.test(obs, correct = FALSE, p = p)$statistic
}

## ---- echo=FALSE, fig.height=3-------------------------------------------
library(ggplot2)
qplot(chisqs, geom = "density")

## ---- echo = FALSE, fig.height=3-----------------------------------------
qplot(chisqs, geom = "density") +
  stat_function(fun = dchisq, args = c(df = 4), col = "tomato")

## ------------------------------------------------------------------------
1 - pchisq(139.64, df = 4)

