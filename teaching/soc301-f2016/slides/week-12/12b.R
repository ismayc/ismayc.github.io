library(tidyr)
library(dplyr)
library(ggplot2)
library(mosaic)
library(readr)

cc <- read_csv("CC-EST2015.csv")

cc_full <- cc %>% 
  select(-(WAC_MALE:NHNAC_FEMALE), -(HWA_MALE:NHAAC_FEMALE)) %>% 
  filter(YEAR == 8, AGEGRP == 0) %>% 
  select(STNAME, CTYNAME, TOT_POP, TOT_MALE, TOT_FEMALE, BA_MALE, BA_FEMALE) %>% 
  mutate(TOT_BA = BA_MALE + BA_FEMALE) %>% 
  mutate(black_perc = TOT_BA / TOT_POP * 100) %>% 
  select(STNAME, CTYNAME, black_perc) %>% 
  rename("state" = STNAME, "county" = CTYNAME)
write_csv(cc_full, "cc_full.csv")  

wash_or <- cc_full %>% filter(state %in% c("Oregon", "Washington")) %>% 
  group_by(state) %>% 
  sample_n(20)

write_csv(wash_or, "wash-or_black.csv")
