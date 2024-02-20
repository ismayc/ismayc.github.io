## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(rglwidget)
options(digits=3, width=100)
knitr::opts_chunk$set(echo = TRUE, fig.align = "center")
library(rgl)
knit_hooks$set(webgl = hook_webgl)
library(dplyr)
library(ggplot2)
library(oilabs)
library(openintro)
library(plotly)

## ----getdata, echo = FALSE, message=FALSE--------------------------------
library(DAAG)
data(allbacks)
books <- allbacks[, c(3, 1, 4)]

## ----parallel_lines_plot, echo = FALSE-----------------------------------
m2 <- lm(weight ~ volume + cover, data = books)
qplot(x = volume, y = weight, color = cover, data = books) +
  geom_abline(intercept = m2$coef[1], slope = m2$coef[2], col = 2) +
  geom_abline(intercept = m2$coef[1] + m2$coef[3], slope = m2$coef[2], col = 4)

## ----diff_slopes_plot, echo = FALSE--------------------------------------
qplot(x = volume, y = weight, color = cover, data = books) +
  stat_smooth(method = "lm", se = FALSE)

## ----interaction_model---------------------------------------------------
m3 <- lm(weight ~ volume + cover + volume:cover, data = books)
summary(m3)

## ----m3_residual_plot----------------------------------------------------
qplot(x = .fitted, y = .stdresid, data = m3)

## ----m3_qqplot-----------------------------------------------------------
qplot(sample = .stdresid, data = m3) + geom_abline(col = "purple")

## ----read_nyc, echo = FALSE----------------------------------------------
nyc <- read.csv("http://andrewpbray.github.io/data/nyc.csv")

## ----struc_nyc-----------------------------------------------------------
head(nyc, 3)
dim(nyc)

## ----scatter3d, echo=FALSE, webgl=TRUE-----------------------------------
library(rgl)
invisible(open3d())
rgl.viewpoint(zoom = .7)
plot3d(x = nyc$Food, y = nyc$Decor, z = nyc$Price, col = "steelblue", 
       xlab = "Food rating", ylab = "Decor Rating", zlab = "Price")

## ----scatter_and_plane3d, echo=FALSE, webgl=TRUE-------------------------
m1 <- lm(Price ~ Food + Decor, data = nyc)
coefs <- m1$coef
#invisible(open3d())
rgl.viewpoint(zoom = .7)
planes3d(coefs["Food"], coefs["Decor"], -1, coefs["(Intercept)"],
         alpha = 0.4, col = "lightgray")

## ----contin_fit----------------------------------------------------------
head(nyc, 3)
m1 <- lm(Price ~ Food + Decor, data = nyc)

## ----summary_m1----------------------------------------------------------
summary(m1)

## ----mod1_plot, echo=FALSE, eval=TRUE, webgl=TRUE------------------------
rgl.viewpoint(zoom = .7)
m1 <- lm(Price ~ Food + Decor, data = nyc)
coefs <- m1$coef
planes3d(coefs["Food"], coefs["Decor"], -1, coefs["(Intercept)"],
         alpha = 0.4, col = "lightgray")

## ----head_nyc------------------------------------------------------------
head(nyc, 3)

## ----add_east------------------------------------------------------------
m2 <- lm(Price ~ Food + Decor + East, data = nyc)
summary(m2)

## ----intersecting_lines, echo = FALSE, eval = TRUE, webgl = TRUE---------
m2 <- lm(Price ~ Food + Decor + East, data = nyc)
colvec <- rep("steelblue", dim(nyc)[1])
colvec[nyc$East == 1] <- "orange"
coefs <- m2$coef
rgl.viewpoint(zoom = .7)
planes3d(coefs["Food"], coefs["Decor"], -1, coefs["(Intercept)"],
         alpha = 0.4, col = "steelblue")
planes3d(coefs["Food"], coefs["Decor"], -1, coefs["(Intercept)"] + coefs["East"],
         alpha = 0.4, col = "orange")

## ----interaction_model2--------------------------------------------------
m3 <- lm(Price ~ Food + Decor + East + Decor:East, data = nyc)
summary(m3)

