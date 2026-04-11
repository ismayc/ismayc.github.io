library(plotly)

analysis <- results %>%
  mutate(differential = wins - projection)

# =============================================================================
# Build the reversion data: pair each season with the previous season
# =============================================================================

reversion <- analysis %>%
  arrange(team, season) %>%
  group_by(team) %>%
  mutate(
    prev_diff = lag(differential),
    prev_season = lag(season),
    prev_wins = lag(wins),
    prev_proj = lag(projection)
  ) %>%
  filter(!is.na(prev_diff)) %>%
  ungroup() %>%
  # Short team name for cleaner labels
  mutate(
    short_team = str_extract(team, "\\w+$"),
    hover_label = paste0(
      "<b>", team, "</b><br>",
      prev_season, ": ", prev_wins, "W (proj ", prev_proj, ") = ",
      ifelse(prev_diff >= 0, "+", ""), prev_diff, "<br>",
      season, ": ", wins, "W (proj ", projection, ") = ",
      ifelse(differential >= 0, "+", ""), differential
    )
  )

# Fit the regression line
model <- lm(differential ~ prev_diff, data = reversion)
slope <- round(coef(model)[2], 3)
r2 <- round(summary(model)$r.squared, 3)
x_range <- range(reversion$prev_diff)
fit_df <- tibble(
  x = seq(x_range[1], x_range[2], length.out = 100),
  y = predict(model, newdata = tibble(prev_diff = x))
)

# =============================================================================
# Plotly scatter
# =============================================================================

p <- plot_ly() %>%
  # Diagonal reference line (y = x, meaning "no change")
  add_trace(
    x = c(-30, 25), y = c(-30, 25),
    type = "scatter", mode = "lines",
    line = list(color = "rgba(150,150,150,0.3)", dash = "dot", width = 1),
    showlegend = FALSE, hoverinfo = "skip"
  ) %>%
  # Zero lines
  add_trace(
    x = c(-30, 25), y = c(0, 0),
    type = "scatter", mode = "lines",
    line = list(color = "rgba(150,150,150,0.4)", width = 0.5),
    showlegend = FALSE, hoverinfo = "skip"
  ) %>%
  add_trace(
    x = c(0, 0), y = c(-30, 25),
    type = "scatter", mode = "lines",
    line = list(color = "rgba(150,150,150,0.4)", width = 0.5),
    showlegend = FALSE, hoverinfo = "skip"
  ) %>%
  # Regression line
  add_trace(
    data = fit_df,
    x = ~x, y = ~y,
    type = "scatter", mode = "lines",
    line = list(color = "#185FA5", width = 2),
    name = paste0("Regression (slope=", slope, ", R\u00B2=", r2, ")"),
    hoverinfo = "skip"
  ) %>%
  # Data points
  add_trace(
    data = reversion,
    x = ~prev_diff, y = ~differential,
    type = "scatter", mode = "markers+text",
    text = ~short_team,
    textposition = "top center",
    textfont = list(size = 9, color = "rgba(100,100,100,0.7)"),
    marker = list(
      size = 9,
      color = ~ifelse(differential > 0, "#0F6E56", "#A32D2D"),
      opacity = 0.7,
      line = list(width = 0.5, color = "white")
    ),
    hovertext = ~hover_label,
    hoverinfo = "text",
    name = "Team-seasons",
    showlegend = FALSE
  ) %>%
  # Quadrant annotations
  add_annotations(
    x = 16, y = -20,
    text = "Big OVER\nthen UNDER\n(reversion)",
    showarrow = FALSE,
    font = list(size = 11, color = "rgba(150,150,150,0.6)")
  ) %>%
  add_annotations(
    x = -22, y = 16,
    text = "Big UNDER\nthen OVER\n(bounce back)",
    showarrow = FALSE,
    font = list(size = 11, color = "rgba(150,150,150,0.6)")
  ) %>%
  add_annotations(
    x = 16, y = 16,
    text = "Sustained\nOVER",
    showarrow = FALSE,
    font = list(size = 11, color = "rgba(150,150,150,0.6)")
  ) %>%
  add_annotations(
    x = -22, y = -20,
    text = "Sustained\nUNDER",
    showarrow = FALSE,
    font = list(size = 11, color = "rgba(150,150,150,0.6)")
  ) %>%
  layout(
    title = list(
      text = paste0(
        "Mean reversion in NBA over/under outcomes<br>",
        "<sup>Each dot is a team-season. ",
        "Hover for details. Green = went OVER, red = went UNDER.</sup>"
      ),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Previous season (wins minus projection)",
      zeroline = FALSE,
      range = c(-30, 25)
    ),
    yaxis = list(
      title = "Next season (wins minus projection)",
      zeroline = FALSE,
      range = c(-30, 25)
    ),
    legend = list(
      x = 0.02, y = 0.98,
      bgcolor = "rgba(255,255,255,0.8)",
      font = list(size = 11)
    ),
    margin = list(t = 80)
  )

p

# =============================================================================
# Summary statistics
# =============================================================================

cat("\n=== Mean Reversion Summary ===\n")
cat(sprintf("Regression slope: %.3f\n", slope))
cat(sprintf("R-squared: %.3f\n", r2))
cat(sprintf("Interpretation: For every +1 win above projection,\n"))
cat(sprintf("  the next year's result drops by %.2f wins on average.\n", abs(slope)))

cat("\n=== After going +12 or more ===\n")
big_over <- reversion %>% filter(prev_diff >= 12)
cat(sprintf("N = %d cases. Went OVER next year: %d (%.0f%%). Went UNDER: %d (%.0f%%).\n",
            nrow(big_over),
            sum(big_over$differential > 0),
            mean(big_over$differential > 0) * 100,
            sum(big_over$differential <= 0),
            mean(big_over$differential <= 0) * 100))

cat("\n=== After going -10 or worse ===\n")
big_under <- reversion %>% filter(prev_diff <= -10)
cat(sprintf("N = %d cases. Went OVER next year: %d (%.0f%%). Went UNDER: %d (%.0f%%).\n",
            nrow(big_under),
            sum(big_under$differential > 0),
            mean(big_under$differential > 0) * 100,
            sum(big_under$differential <= 0),
            mean(big_under$differential <= 0) * 100))