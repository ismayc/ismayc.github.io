library(tidyr)
library(dplyr)
library(readr)
zinc_conc <- read.delim("zinc_conc.txt") %>% 
  rownames_to_column(var = "loc_id") %>% 
  mutate(loc_id = as.numeric(loc_id))
zinc_tidy <- zinc_conc %>% gather(key = "location", value = "concentration", -loc_id) %>% 
  arrange(loc_id)
write_csv(zinc_tidy, "zinc_tidy.csv")
