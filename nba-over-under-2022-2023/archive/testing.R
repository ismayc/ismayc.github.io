testing <- df %>% 
  ungroup() %>% 
  select(1, 3, 7:10) %>% 
  mutate(diff = `Winning % In Remaining Games Needed` - `Current Win %`) %>% 
  mutate(ratio_diff = ifelse( 
    (`Winning % In Remaining Games Needed` <= 0) | 
      (`Winning % In Remaining Games Needed` > 100), 
    NA_real_, 
    abs(diff / `Current Win %`))) %>% 
  mutate(ratio_diff = ifelse(
    str_detect(string = Team, pattern = "Timber|Magic"),
    NA_real_,
    ratio_diff
  )) %>% 
  # mutate(scaled = ifelse(
  #   is.na(ratio_diff),
  #   100,
  #   (99 - 55) / diff(range(ratio_diff, na.rm = TRUE, finite = TRUE)) * (ratio_diff - max(ratio_diff, na.rm = TRUE)) + 99
  # )) %>% 
  metan::resca(ratio_diff,
               new_min = 55,
               new_max = 96) %>% 
  rename(generated_prob = ratio_diff_res) %>% 
  mutate(generated_prob = replace_na(generated_prob, 100)) %>% 
  mutate(generated_prob = replace(generated_prob, 
                                  str_detect(string = Team, pattern = "Timber"),
                                  99)) %>% 
  mutate(generated_prob = replace(generated_prob, 
                                  str_detect(string = Team, pattern = "Magic"),
                                  97)) %>% 
  arrange(desc(generated_prob)) 
View(testing)
