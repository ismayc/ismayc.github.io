## ----setup, include=FALSE------------------------------------------------
library(knitr)
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(mosaic)

## ----simgender, echo = FALSE, cache=TRUE, fig.height=4, fig.width=9, message=FALSE, warning=FALSE----
gender <- rep(c("M", "F"), each = 24)
promote <- rep(c("Yes", "No"), c(35, 13))
D <- rep(NA, 10000)
d_obs <- 21/24 - 14/24
for(i in 1:10000) {
  newgen <- sample(gender)
  tab <- table(newgen, promote)
  D[i] <- diff(tab[, 2]/24)
}

qplot(x = D, fill = I("steelblue"), colour = I("black"), 
  main = "Null Distribution", bins = 12) +
  geom_vline(xintercept = d_obs, colour = "goldenrod")
ggsave("../figs/gender_promote.png", device = "png")

