library(tidyverse)
library(forcats)
library(tidytext) # for reorder_within / scale_x_reordered

# =============================================================================
# Team-by-team O/U patterns across 6 seasons
# =============================================================================

# (Uses the `analysis` data frame from ou_playoff_analysis.R)
# Run that script first, or source it. This script extends it.

# If you haven't run it yet, source it:
# source("ou_playoff_analysis.R")

# --- Team-level summary ---
team_summary <- analysis %>%
  group_by(team) %>%
  summarize(
    seasons = n(),
    overs = sum(over_under == "OVER"),
    unders = sum(over_under != "OVER"),
    total_diff = sum(differential),
    avg_diff = round(mean(differential), 1),
    .groups = "drop"
  ) %>%
  mutate(
    category = case_when(
      overs >= 4 ~ "Vegas underrates",
      unders >= 4 ~ "Vegas overrates",
      TRUE ~ "Coin flip"
    ),
    category = factor(category, levels = c("Vegas underrates", "Coin flip", "Vegas overrates"))
  ) %>%
  arrange(desc(total_diff))

# --- Plot 5: Team total differential (lollipop) ---
p5 <- team_summary %>%
  mutate(team = fct_reorder(team, total_diff)) %>%
  ggplot(aes(x = total_diff, y = team, color = category)) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "gray70") +
  geom_segment(aes(x = 0, xend = total_diff, yend = team), linewidth = 0.8) +
  geom_point(size = 3) +
  geom_text(
    aes(label = paste0(overs, "-", unders),
        hjust = if_else(total_diff >= 0, -0.4, 1.4)),
    size = 2.8, show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Vegas underrates" = "#0F6E56",
      "Coin flip" = "#185FA5",
      "Vegas overrates" = "#A32D2D"
    ),
    name = NULL
  ) +
  labs(
    title = "Six-year cumulative wins vs projection by team",
    subtitle = "Label shows OVER-UNDER record. Positive = Vegas underrated them.",
    x = "Total wins above/below projection (6 seasons)", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

print(p5)

# --- Plot 6: Season-by-season heatmap per team ---
team_order <- team_summary %>% arrange(desc(total_diff)) %>% pull(team)

p6 <- analysis %>%
  mutate(
    team = factor(team, levels = rev(team_order)),
    label = if_else(differential >= 0, paste0("+", differential), as.character(differential))
  ) %>%
  ggplot(aes(x = season, y = team, fill = differential)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label), size = 2.4, color = "white", fontface = "bold") +
  scale_fill_gradient2(
    low = "#A32D2D", mid = "#888780", high = "#0F6E56",
    midpoint = 0, name = "Wins vs projection"
  ) +
  labs(
    title = "Every team, every season: wins minus projection",
    subtitle = "Green = OVER, red = UNDER. Sorted by 6-year total.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank())

print(p6)

# --- Plot 7: Mean reversion ---
# After a team goes +12 or more, what happens next year?
reversion <- analysis %>%
  arrange(team, season) %>%
  group_by(team) %>%
  mutate(
    prev_diff = lag(differential),
    prev_season = lag(season)
  ) %>%
  filter(!is.na(prev_diff)) %>%
  ungroup()

p7 <- reversion %>%
  ggplot(aes(x = prev_diff, y = differential)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray80", linewidth = 0.3) +
  geom_point(aes(color = differential > 0), alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#185FA5", linewidth = 0.8) +
  scale_color_manual(values = c("TRUE" = "#0F6E56", "FALSE" = "#A32D2D"), guide = "none") +
  annotate("text", x = 15, y = -22, label = "Big OVER\nthen UNDER\n(reversion)", 
           size = 3, color = "gray50", hjust = 0.5) +
  annotate("text", x = -22, y = 15, label = "Big UNDER\nthen OVER\n(bounce back)", 
           size = 3, color = "gray50", hjust = 0.5) +
  labs(
    title = "Mean reversion: this year's diff vs last year's diff",
    subtitle = "Negative slope confirms teams regress toward their projection",
    x = "Previous season (wins - projection)", y = "Next season (wins - projection)"
  ) +
  theme_minimal(base_size = 12)

print(p7)

# Print the regression
model <- lm(differential ~ prev_diff, data = reversion)
cat("\n=== Mean reversion regression ===\n")
cat(sprintf("Slope: %.3f (each +1 win above projection predicts %.1f fewer next year)\n",
            coef(model)[2], -coef(model)[2]))
cat(sprintf("R-squared: %.3f\n", summary(model)$r.squared))

# --- Actionable summary table ---
cat("\n\n=== PICK GUIDE FOR NEXT SEASON ===\n\n")

cat("CONSISTENT OVER teams (bet OVER with confidence):\n")
team_summary %>% filter(category == "Vegas underrates") %>%
  mutate(rec = paste0("  ", team, ": ", overs, "/", seasons, " OVER, avg ", 
                       sprintf("%+.1f", avg_diff), " wins/yr")) %>%
  pull(rec) %>% walk(cat, "\n")

cat("\nCONSISTENT UNDER teams (bet UNDER with confidence):\n")
team_summary %>% filter(category == "Vegas overrates") %>%
  arrange(total_diff) %>%
  mutate(rec = paste0("  ", team, ": ", unders, "/", seasons, " UNDER, avg ", 
                       sprintf("%+.1f", avg_diff), " wins/yr")) %>%
  pull(rec) %>% walk(cat, "\n")

cat("\nCOIN FLIP teams (save your high-confidence points for elsewhere):\n")
team_summary %>% filter(category == "Coin flip") %>%
  mutate(rec = paste0("  ", team, ": ", overs, "-", unders, " split, avg ",
                       sprintf("%+.1f", avg_diff))) %>%
  pull(rec) %>% walk(cat, "\n")
