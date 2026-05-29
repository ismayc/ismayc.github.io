party <- c(rep("democrat", 285), rep("republican", 215))
opinion <- c(rep("favor", 138), rep("indifferent", 83), rep("opposed", 64),
  rep("favor", 64), rep("indifferent", 67), rep("opposed", 84))
party_tax <- data.frame(party, opinion)
write.csv(party_tax, "data/party_tax.csv", row.names = FALSE)