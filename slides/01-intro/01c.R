## ----setup, include=FALSE------------------------------------------------
pkg <- c("dplyr", "readr", "mosaic")

new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg, repos = "http://cran.rstudio.com")
}

lapply(pkg, library, character.only = TRUE)

## ----load_data-----------------------------------------------------------
url <- "http://www.usatoday.com/sports/mlb/salaries/"
salaries <- read_csv("mlb_salaries.csv") %>%
  select(NAME, TEAM, POS, SALARY)
salaries_trim <- salaries %>% 
  filter(TEAM %in% c("LAD", "NYY", "TB", "ARI"))
salaries_summary <- salaries_trim %>% group_by(TEAM) %>%
  summarize(mean_sal = mean(SALARY), count_team = n())
grand_mean <- salaries %>% summarize(mean(SALARY))

## ----srs-----------------------------------------------------------------
n <- 40
set.seed(20160129)
mean_srs <- salaries %>% 
  sample_n(n) %>%
  summarize(mean_srs_salary = mean(SALARY))

## ----strat---------------------------------------------------------------
strat_n <- 10
mean_strat_by_team <- salaries %>% group_by(TEAM) %>%
  sample_n(strat_n) %>%
  summarize(mean_by_team = mean(SALARY))
mean_strat <- mean_strat_by_team %>%
  summarize(mean(mean_by_team))

## ----comp, echo=FALSE, fig.align="center", fig.height=4------------------
srs_mean <- function(df){
  df %>% 
    summarize(mean_srs_salary = mean(SALARY))
} 

strat_mean <- function(df){
  mean_strat_by_team <- df %>% group_by(TEAM) %>%
    sample_n(strat_n) %>%
    summarize(mean_by_team = mean(SALARY))
  mean_strat <- mean_strat_by_team %>%
    summarize(mean(mean_by_team))
  mean_strat
}

# sample repeatedly (1000 times) 150 rows of df and store in a list
# apply the custom function to each sample in the list,
# bind rows together and create an index column, all in a "pipe":

srs_sim <- replicate(1000, sample_n(salaries, n), simplify = FALSE) %>%
  lapply(., srs_mean) %>% 
  bind_rows %>%
  mutate(replicate = 1:n())
  

strat_sim <- replicate(1000, sample_n(group_by(salaries, TEAM), strat_n), simplify = FALSE) %>%
  lapply(., strat_mean) %>% 
  bind_rows
  
sim <- bind_cols(srs_sim, strat_sim) %>%
  select(replicate, everything())

names(sim) <- c("replicate", "SRS", "STR")
densityplot(~SRS + STR, data = sim, auto.key = TRUE)

