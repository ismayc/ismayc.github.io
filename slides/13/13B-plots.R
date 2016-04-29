library(ggplot2)
library(dplyr)
homedir <- "/Users/Chester/Google Drive/ismayc.github.io/teaching/diagrams/"

# 1
response <- rnorm(1000)
qplot(x = response, geom = "histogram", binwidth = 0.5, col = I("white")) + theme(text = element_text(size = 30))
ggsave(paste0(homedir, "01-histogram.png"), dpi = 1000)

# 2
response <- c(rnorm(1000, mean = 10, sd = 2), rnorm(1000, mean = 5, sd = 1))
explanatory <- c(rep("group1", 1000), rep("group2", 1000))
dataframe <- data.frame(explanatory, response) 
qplot(x = explanatory, y = response, data = dataframe, geom = "boxplot") + theme(text = element_text(size = 30))
ggsave(paste0(homedir, "02-side_by_side_boxplot.png"), dpi = 1000)

# 3
differences <- c(rnorm(500))
qplot(x = differences, geom = "histogram", binwidth = 0.5, col = I("white")) + theme(text = element_text(size = 30))
ggsave(paste0(homedir, "03-differences_histogram.png"), dpi = 600)


# 7
response <- c(rep("group1", 57), rep("group2", 75), rep("group3", 40), rep("group4", 50))
qplot(x = response) + theme(text = element_text(size = 30))
ggsave(paste0(homedir, "07-single-bar.png"), dpi = 1000)

# 9
response <- c(rep("success", 73), rep("failure", 27))
qplot(x = response) + theme(text = element_text(size = 30))
ggsave(paste0(homedir, "09-single-bar.png"), dpi = 1000)
