candy_colors <- c(rep("brown", 224), rep("yellow", 119), rep("orange", 130), rep("green", 48), rep("coffee", 59))
candies <- data.frame(candy_colors)
write.csv(candies, "data/candies.csv", row.names = FALSE)