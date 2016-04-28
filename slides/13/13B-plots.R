library(ggplot2)
library(dplyr)
homedir <- "/Users/Chester/Google Drive/ismayc.github.io/teaching/diagrams/"

# 1
response <- rnorm(1000)
dataframe <- data.frame(dataframe = response)
qplot(x = response, geom = "histogram", binwidth = 0.5, col = I("white")) + theme(text = element_text(size = 30))
ggsave(paste0(homedir, "01-histogram.png"), dpi = 1000)

# 7
response <- c(rep("group1", 57), rep("group2", 75), rep("group3", 40), rep("group4", 50))
qplot(x = response) + theme(text = element_text(size = 30))
ggsave(paste0(homedir, "07-single-bar.png"), dpi = 1000)

# 9
response <- c(rep("success", 73), rep("failure", 27))
qplot(x = response) + theme(text = element_text(size = 30))
ggsave(paste0(homedir, "09-single-bar.png"), dpi = 1000)
