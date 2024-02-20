# MATH 141
Chester Ismay  



## {.flexbox .vcenter}

![plicker](plicker_roster.png)

# Simple Random Sampling <br> versus <br> Stratified Sampling {.flexbox .vcenter}

## 2015 MLB player salaries

Let's investigate the average salary of four teams:

  - Los Angeles Dodgers
  - New York Yankees
  - Tampa Bay Devil Rays
  - Arizona Diamondbacks


```r
url <- "http://www.usatoday.com/sports/mlb/salaries/"
salaries <- read_csv("mlb_salaries.csv") %>%
  select(NAME, TEAM, POS, SALARY)
salaries_trim <- salaries %>% 
  filter(TEAM %in% c("LAD", "NYY", "TB", "ARI"))
salaries_summary <- salaries_trim %>% group_by(TEAM) %>%
  summarize(mean_sal = mean(SALARY), count_team = n())
grand_mean <- salaries %>% summarize(mean(SALARY))
```

---

- 120 player salaries available for those four teams
- The mean salary for ALL 120 players on those four teams is $$\mu = \$4,214,614$$

- Remember, this is a summary value for the **population**.  We hardly ever have the whole population to work with.

## How do SRS and Stratified Sampling compare in estimating $\mu$?


```r
n <- 40
set.seed(20160129)
mean_srs <- salaries %>% 
  sample_n(n) %>%
  summarize(mean_srs_salary = mean(SALARY))
```

- Suppose we select 40 players at random from the 120 total

- $\bar{x}_{SRS} = \$4,248,380$

## Now for Stratified


```r
strat_n <- 10
mean_strat_by_team <- salaries %>% group_by(TEAM) %>%
  sample_n(strat_n) %>%
  summarize(mean_by_team = mean(SALARY))
mean_strat <- mean_strat_by_team %>%
  summarize(mean(mean_by_team))
```

- Let's select 10 players from each of the 4 teams

- $\bar{x}_{STRAT} = \$3,998,016$

- SRS: Absolute bias of \$33,766.27

- Stratified: Absolute bias of \$216,597.9

### So is this evidence that simple random sampling is clearly better?

## Not so fast!

<img src="01C-experiments_files/figure-html/comp-1.png" title="" alt="" style="display: block; margin: auto;" />

#

## {.flexbox .vcenter}

<img src="../figs/corrs1.png" alt="corrs1" width="850">


## {.flexbox .vcenter}

<img src="../figs/corrs2.png" alt="corrs2" width="850">


## If you learn one thing in this class... {.flexbox .vcenter}

<img src="../figs/xkcd-correlation.png" alt="corr" width="850">

# Plicker time!

# Practice

## Practice

1. Find your numerical pair
2. Introduce yourself (name, year, major, hometown)
3. Discuss the problems on the handout and record your thoughts.

## Principles of Experimental Design {.build}

**Control**: Compare treatment of interest to a control group.

**Randomization**: Randomly assign subjects to treatments.

**Replication**: Within a study, replicate by collecting a sufficiently large sample. Or replicate the entire study.

**Blocking**: If there are variables that are known or suspected to affect the response variable, first group subjects into blocks based on these variables, and then randomize cases within each block to treatment groups.


## Replication

<img src="../figs/psych.png" alt="psych" width="750">

<!--
## Blocking

A study is designed to test the effect of light level and noise level on exam performance of students. The researcher also believes that light and noise levels might have different effects on males and females, so wants to make sure both genders are represented equally under different conditions. Which of the below is correct?

1. There are 3 explanatory variables (light, noise, gender) and 1 response variable (exam performance)
2. There are 2 explanatory vars (light and noise), 1 blocking var (gender), and 1 response var (exam performance)
3. There is 1 explanatory var (gender) and 3 response vars (light, noise, exam performance)
4. There are 2 blocking vars (light and noise), 1 explanatory var (gender), and 1 response var (exam performance)
-->

## Other key ideas {.build}

**Placebo**: fake treatment, often used as the control group for medical studies

**Placebo effect**: experimental units showing improvement simply because they believe they are receiving a special treatment

**Blinding**: when experimental units do not know whether they are in the control or treatment group

**Double-blind**: when both the experimental units and the researchers do not know who is in the control and who is in the treatment group

<!--
## Consider acupuncture {.build}

<img src="../figs/acupuncture.png" alt="acupuncture" width="500">

How do you test if acupuncture reduces pain?

"Sham acupuncture" is a good control.
-->



