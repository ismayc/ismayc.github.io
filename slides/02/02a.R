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
options(digits = 2)

## ------------------------------------------------------------------------
x <- c(8, 11, 7, 7, 8, 11, 9, 6, 10, 7, 9)

## ------------------------------------------------------------------------
mean(x)

## ------------------------------------------------------------------------
sort(x)
median(x)

## ------------------------------------------------------------------------
table(x)

## ------------------------------------------------------------------------
mode(x)

## ------------------------------------------------------------------------
x - mean(x)
(x - mean(x))^2
sum((x - mean(x))^2) / (length(x) - 1)
var(x)

## ------------------------------------------------------------------------
sqrt(var(x))
sd(x)

## ----IQR-----------------------------------------------------------------
sort(x)
IQR(x)

## ----range---------------------------------------------------------------
max(x) - min(x)
range(x)
diff(range(x))

## ----create-y, echo=FALSE------------------------------------------------
y <- x
y[2] <- 37

## ----compare_x&y---------------------------------------------------------
x
y
var(x)
var(y)

## ----distxy--------------------------------------------------------------
IQR(x)
IQR(y)
diff(range(x))
diff(range(y))

