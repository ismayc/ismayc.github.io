## ----setup, include=FALSE------------------------------------------------
# List of useful packages
pkg <- c("dplyr", "ggplot2", "knitr")

# Check if packages are not installed and assign the
# names of the uninstalled packages to the variable new.pkg
new.pkg <- pkg[!(pkg %in% installed.packages())]

# If there are any packages in the list that aren't installed,
# install them
if (length(new.pkg)) {
  install.packages(new.pkg, repos = "http://cran.rstudio.com")
}

# Load the packages into the current environment
library(knitr)
library(dplyr)
library(ggplot2)

# Set number of digits to display
options(digits = 3)

## ---- echo=FALSE, results="asis"-----------------------------------------
set.seed(20160201)
gpa <- round(rnorm(10, mean = 3, sd = 0.5), 2)
kable(data.frame(t(gpa)), align = c("c", "c"))

## ----dotplot, echo=FALSE, fig.height=1-----------------------------------
data.frame(gpa) %>% ggplot(aes(x = gpa)) + 
  geom_dotplot(binwidth = 0.05) +
  scale_y_continuous(name = "", breaks = NULL)

## ----hist, echo=FALSE, fig.height=2--------------------------------------
data.frame(gpa) %>% ggplot(aes(x = gpa)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill = "goldenrod")

## ----densityplot, echo=FALSE, fig.height=2-------------------------------
data.frame(gpa) %>% ggplot(aes(x = gpa)) + 
  geom_density()

## ----boxplot, echo=FALSE, fig.height=1-----------------------------------
data.frame(gpa) %>% ggplot(aes(x = '', y = gpa)) + 
  geom_boxplot() +
  coord_flip()

