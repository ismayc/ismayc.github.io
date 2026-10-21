# Charts and summaries for the in-season page (make_plots.Rmd), in the site's
# scorer's-book theme. Sourced by make_plots.Rmd after 03-data-wrangling.R.
#
# Color carries one meaning everywhere:
#   Over is blue, Under is red. Pale = on pace (could still change),
#   solid = clinched. Players get fixed categorical colors (validated with the
#   dataviz palette checker against the #fafbf7 sheet; three hues are below 3:1
#   contrast, so every player line is also labeled directly).

library(plotly)

ou_col <- list(
  ink = "#1c2a44", ink_soft = "#56627a", rule = "#cdd6c6", sheet = "#fafbf7",
  over = "#2457a6", over_pale = "#9db6dc",
  under = "#b8322f", under_pale = "#e3a6a3",
  alt = "#6b5a9e",
  right = "#d5e6cf", wrong = "#f1d2cf"
)
ou_state_colors <- c(
  "Over, clinched" = ou_col$over, "Over, on pace" = ou_col$over_pale,
  "Under, on pace" = ou_col$under_pale, "Under, clinched" = ou_col$under
)
ou_player_palette <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                       "#e87ba4", "#008300", "#6250d6", "#e34948")
# One color per person on every season's page, in alphabetical order so the
# validated adjacent pairs hold. Jenelle (2020-21 to 2022-23) adds a ninth
# hue, #ad1457, which passes between Jake and Mary and is at least 15
# (OKLab) from all eight.
ou_player_colors <- c(Adonis = "#2a78d6", Andy = "#eb6834", Chester = "#1baf7a",
                      Jake = "#eda100", Jenelle = "#ad1457", Mary = "#e87ba4",
                      Mike = "#008300", Phil = "#6250d6", Ryan = "#e34948")
player_colors <- function(players) {
  players <- sort(unique(players))
  cols <- ou_player_colors[players]
  # A new player takes the first palette color nobody this season is using
  spare <- setdiff(ou_player_palette, cols)
  cols[is.na(cols)] <- spare[seq_len(sum(is.na(cols)))]
  setNames(unname(cols), players)
}
ou_font <- '"Source Sans 3", "Helvetica Neue", Arial, sans-serif'

theme_ou <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(color = ou_col$ink),
      strip.text = element_text(size = 9, face = "bold", hjust = 0,
                                color = ou_col$ink),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e6eae2", linewidth = 0.3),
      axis.text = element_text(color = ou_col$ink_soft, size = 8),
      panel.spacing = unit(0.6, "lines"),
      legend.position = "top"
    )
}

ou_plotly <- function(p) {
  p |>
    config(displayModeBar = FALSE) |>
    layout(font = list(family = ou_font, color = ou_col$ink),
           paper_bgcolor = ou_col$sheet, plot_bgcolor = ou_col$sheet)
}

# Over/Under state at each game: clinched once it can no longer change
ou_state <- function(wins, games, line, n_games = 82) {
  case_when(
    wins > line ~ "Over, clinched",
    wins + (n_games - games) < line ~ "Under, clinched",
    wins - line * games / n_games >= 0 ~ "Over, on pace",
    TRUE ~ "Under, on pace"
  )
}

# Facet labels carry the line, e.g. "Atlanta Hawks (46.5)", and every team
# chart uses the same order: furthest above the line's pace first.
team_order <- function(join_with_projections, n_games = 82) {
  join_with_projections |>
    group_by(`Team Name`) |>
    slice_max(`Game Number`, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(vs_pace = current_wins - win_projection * `Game Number` / n_games,
           # Nicknames keep five facets per row from colliding
           nickname = str_remove(`Team Name`,
                                 "^(Los Angeles|New York|New Orleans|Oklahoma City|San Antonio|Golden State|Portland|[A-Za-z]+) "),
           label = sprintf("%s (%.1f)", nickname, win_projection)) |>
    arrange(desc(vs_pace)) |>
    select(`Team Name`, label, vs_pace)
}

# Recommendation 2: wins above or below the pace needed to beat the line.
# by = "game" plots every game; by = "week" plots each team on Sundays.
pace_chart <- function(join_with_projections, n_games = 82, by = c("game", "week"),
                       standings_grid = NULL, projections = NULL) {
  by <- match.arg(by)
  ord <- team_order(join_with_projections, n_games)
  if (by == "game") {
    df <- join_with_projections |>
      ungroup() |>
      transmute(`Team Name`, x = `Game Number`, wins = current_wins,
                games = `Game Number`, line = win_projection, text = `Game Result`)
  } else {
    df <- standings_grid |>
      ungroup() |>
      filter(lubridate::wday(date) == 1) |>
      transmute(`Team Name` = team, x = date, wins = current_wins,
                games = current_wins + current_losses,
                line = projections$win_projection[match(team, projections$team)],
                text = paste0(format(date, "%b %d"), ": ", `Current Record`))
  }
  df <- df |>
    mutate(
      `Wins vs line pace` = round(wins - line * games / n_games, 1),
      State = factor(ou_state(wins, games, line, n_games),
                     levels = names(ou_state_colors))
    ) |>
    left_join(ord |> select(`Team Name`, label), by = "Team Name") |>
    mutate(label = factor(label, levels = ord$label))
  p <- ggplot(df, aes(x = x, y = `Wins vs line pace`, text = text)) +
    geom_hline(yintercept = 0, color = ou_col$ink, linewidth = 0.4) +
    geom_line(aes(group = label), color = ou_col$rule, linewidth = 0.5) +
    geom_point(aes(color = State), size = 1.1) +
    scale_color_manual(values = ou_state_colors, drop = FALSE, name = NULL) +
    facet_wrap(~ label, ncol = 5) +
    labs(x = NULL, y = NULL) +
    theme_ou() +
    theme(legend.position = "none")
  ggplotly(p, tooltip = c("text", "y"), height = 1050) |>
    layout(margin = list(t = 30, l = 40, r = 10)) |>
    ou_plotly()
}

# The Over/Under color key, as HTML above a chart (shared by the team charts)
state_legend <- function() {
  swatch <- function(col, label) {
    tags$span(style = "margin-right:16px;white-space:nowrap",
              tags$span(style = sprintf(
                "display:inline-block;width:10px;height:10px;border-radius:50%%;background:%s;margin-right:5px;vertical-align:-1px",
                col)), label)
  }
  tags$p(style = "font-size:0.9rem;margin:4px 0 8px",
         purrr::imap(ou_state_colors, \(col, label) swatch(col, label)))
}

# Recommendation 1: each player's chance to win the pool, and to finish in
# the top 4 (the pool's playoffs), as of every date so far. Each team's
# remaining games are simulated from its record so far, pulled toward its
# Vegas line (worth prior_games games), so early results do not swing it.
# Each simulation also draws the team's true win rate from a beta
# distribution, with the line and record counted at strength_weight times
# their games. A fixed rate was overconfident in a backtest on 2021-22 to
# 2025-26 (teams given 88% to finish over did so 69% of the time); a weight
# of 0.5 scored best (backtest/prior_weight.R and backtest/betabin.R).
#
# With `cache` (an .rds path), dates already simulated are reused: with the
# same seed every date's draws are the same, so a date only changes if its
# records do. The cache is rebuilt whenever the picks, lines, or settings
# change. A date takes about 4 seconds at 100,000 simulations.
chance_by_date <- function(standings_grid, projections, picks, n_sims = 100000,
                           prior_games = 20, strength_weight = 0.5,
                           n_games = 82, seed = 2027, cache = NULL) {
  set.seed(seed)
  players <- sort(unique(picks$player))
  signed <- picks |>
    mutate(s = if_else(choice == "OVER", wage, -wage)) |>
    select(team, player, s) |>
    pivot_wider(names_from = player, values_from = s) |>
    arrange(team)
  teams <- signed$team
  S <- as.matrix(signed[, players])
  sgn <- sign(S)
  w15 <- sgn * (abs(S) == 15)
  line <- projections$win_projection[match(teams, projections$team)]
  grid <- standings_grid |>
    ungroup() |>
    filter(team %in% teams) |>
    select(team, date, current_wins, current_losses)
  dates <- sort(unique(grid$date))

  settings <- rlang::hash(list(method = 2, teams, S, line, n_sims, prior_games,
                               strength_weight, n_games, seed))
  saved <- if (!is.null(cache) && file.exists(cache)) readRDS(cache)
  if (is.null(saved) || !identical(saved$settings, settings)) {
    saved <- list(settings = settings, results = tibble())
  }

  # Common random numbers: the same uniform draws per simulation and team on
  # every date, so day-to-day changes come from games, not simulation noise
  U <- matrix(runif(n_sims * length(teams)), n_sims)
  U_rate <- matrix(runif(n_sims * length(teams)), n_sims)
  out <- vector("list", length(dates))
  for (k in seq_along(dates)) {
    d <- grid |> filter(date == dates[k])
    w <- d$current_wins[match(teams, d$team)]
    l <- d$current_losses[match(teams, d$team)]
    w[is.na(w)] <- 0; l[is.na(l)] <- 0
    records <- rlang::hash(list(w, l))
    hit <- if (nrow(saved$results)) {
      filter(saved$results, date == dates[k], records == !!records)
    }
    if (length(hit) && nrow(hit) == length(players)) {
      out[[k]] <- hit
      next
    }
    g <- w + l
    rem <- pmax(n_games - g, 0)
    q <- line / n_games
    a <- strength_weight * (w + prior_games * q)
    b <- strength_weight * (l + prior_games * (1 - q))
    p <- qbeta(U_rate, rep(a, each = n_sims), rep(b, each = n_sims))
    final <- matrix(qbinom(U, rep(rem, each = n_sims), p),
                    n_sims) +
      matrix(w, n_sims, length(teams), byrow = TRUE)
    dir <- ifelse(final > matrix(line, n_sims, length(teams), byrow = TRUE), 1, -1)
    pts <- dir %*% S
    key <- pts * 1e4 + ((dir %*% sgn + length(teams)) / 2) * 1e2 + (dir %*% w15 + 3) / 2
    # Place = 1 + players strictly ahead (ties share a place)
    rk <- sapply(seq_along(players), \(i) 1L + rowSums(key > key[, i]))
    top <- rk == 1
    out[[k]] <- tibble(date = dates[k], player = players,
                       p_first = colMeans(top / rowSums(top)),
                       p_top4 = colMeans(rk <= 4),
                       # Once every game is played, every simulation agrees
                       final_rank = if (all(rem == 0)) as.integer(rk[1, ]) else NA_integer_,
                       records = records)
  }
  results <- bind_rows(out)
  if (!is.null(cache)) saveRDS(list(settings = settings, results = results), cache)
  select(results, -records)
}

chance_chart <- function(chances, measure = c("p_first", "p_top4")) {
  measure <- match.arg(measure)
  players <- sort(unique(chances$player))
  cols <- player_colors(players)
  last <- chances |> filter(date == max(date)) |>
    mutate(v = .data[[measure]],
           place = if ("final_rank" %in% names(chances)) final_rank else NA_integer_) |>
    arrange(desc(v), place)
  # Spread end labels so they never overlap (at least 4.5 points apart) and
  # stay inside 0-100%: push down from the top, then back up from 0
  gap <- 0.045
  y <- last$v
  for (i in seq_along(y)[-1]) y[i] <- min(y[i], y[i - 1] - gap)
  y[length(y)] <- max(y[length(y)], 0)
  for (i in rev(seq_along(y))[-1]) y[i] <- max(y[i], y[i + 1] + gap)
  last$label_y <- y
  # After the last game the chances are certainties, so the labels give each
  # player's final place instead of 100% and 0%
  is_final <- "final_rank" %in% names(last) && !anyNA(last$final_rank)
  ordinal <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th",
                                  c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]))
  # A place shared after every tiebreaker reads "T-2nd"
  tied <- if (is_final) duplicated(last$final_rank) | duplicated(last$final_rank, fromLast = TRUE)
  last$text <- if (is_final) sprintf("%s, %s%s", last$player, ifelse(tied, "T-", ""),
                                     ordinal(last$final_rank)) else
    sprintf("%s %.0f%%", last$player, last$v * 100)
  # With only a few days so far, a line has too few points to show
  mode <- if (n_distinct(chances$date) < 5) "lines+markers" else "lines"
  span <- as.numeric(max(chances$date) - min(chances$date))
  short <- span < 45
  # Whole days apart (milliseconds), about four ticks
  day_step <- 86400000 * max(1, ceiling((span + 1) / 4))
  plt <- plot_ly()
  for (pl in last$player) {
    d <- chances |> filter(player == pl)
    args <- list(plt, data = d, x = ~date, y = d[[measure]], name = pl,
                 type = "scatter", mode = mode,
                 line = list(color = cols[[pl]], width = 2),
                 hovertemplate = paste0(pl, ": %{y:.1%}<extra>%{x|%b %-d}</extra>"))
    # Marker settings only with markers, or plotly adds them to every line
    if (mode == "lines+markers") args$marker <- list(color = cols[[pl]], size = 8)
    plt <- do.call(add_trace, args)
  }
  ref <- if (measure == "p_first") 1 / length(players) else 4 / length(players)
  if (is_final) {
    plt <- plt |>
      add_annotations(x = max(last$date), y = max(last$label_y) + 0.045,
                      text = "<b>Final</b>", xanchor = "left", xshift = 6,
                      showarrow = FALSE, font = list(size = 12, color = ou_col$ink))
  }
  plt |>
    add_annotations(data = last, x = ~date, y = ~label_y, text = ~text,
                    xanchor = "left", xshift = 6, showarrow = FALSE,
                    font = list(size = 12, color = ou_col$ink)) |>
    layout(
      yaxis = list(title = "", tickformat = ".0%",
                   range = c(0, max(1, max(last$label_y) + if (is_final) 0.08 else 0.03)),
                   gridcolor = "#e6eae2", zeroline = FALSE),
      # Month names only, so the dates don't collide on a phone; in the
      # first weeks that repeated "Oct", so short spans show days
      xaxis = list(title = "", gridcolor = "#e6eae2",
                   nticks = if (short) NULL else 6,
                   dtick = if (short) day_step else NULL,
                   tickformat = if (short) "%b %-d" else "%b",
                   range = c(min(chances$date), max(chances$date) + 1)),
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                         y0 = ref, y1 = ref,
                         line = list(color = ou_col$ink_soft, dash = "dot", width = 1))),
      legend = list(orientation = "h", x = 0, y = -0.08, xanchor = "left",
                    yanchor = "top"),
      margin = list(r = 110, t = 10),
      hovermode = "x unified"
    ) |>
    ou_plotly()
}

# Recommendation 4: latest standings only, one column per tiebreaker
standings_table <- function(projected_score) {
  latest <- projected_score |>
    filter(Date == max(Date)) |>
    arrange(desc(`Projected Total Points`), desc(`Number Correct`),
            desc(`Number (Wage 15) Correct`), desc(`Number (Wage 14) Correct`),
            desc(`Number (Wage 13) Correct`), desc(`Number (Wage 12) Correct`)) |>
    transmute(Rank = row_number(), Player,
              `Projected points` = `Projected Total Points`,
              `Picks correct` = `Number Correct`,
              `15s` = `Number (Wage 15) Correct`,
              `14s` = `Number (Wage 14) Correct`,
              `13s` = `Number (Wage 13) Correct`,
              `12s` = `Number (Wage 12) Correct`)
  css <- tags$style(HTML(sprintf(
    "#proj_score table.dataTable tbody tr:nth-child(4) td { border-bottom: 3px solid %s !important; }",
    ou_col$ink)))
  tagList(css, DT::datatable(latest, rownames = FALSE, elementId = "proj_score",
                             options = list(dom = "t", pageLength = nrow(latest),
                                            ordering = FALSE)))
}

# Recommendation 4: yesterday's swing per player, plus the teams behind it
swing_chart <- function(rows_today) {
  net <- rows_today |>
    group_by(Player) |>
    summarize(net = sum(`Change in Points Since Yesterday`),
              detail = paste(sprintf("%s %+d", Team, `Change in Points Since Yesterday`),
                             collapse = "<br>"),
              .groups = "drop") |>
    arrange(net) |>
    mutate(Player = factor(Player, levels = Player))
  plot_ly(net, y = ~Player, x = ~net, type = "bar", orientation = "h",
          marker = list(color = ou_col$ink),
          text = ~sprintf("%+d", net), textposition = "outside",
          textfont = list(color = ou_col$ink),
          hovertemplate = "%{y}: %{x:+d}<br>%{customdata}<extra></extra>",
          customdata = ~detail, height = 60 + 34 * nrow(net)) |>
    layout(xaxis = list(title = "Change in projected points since yesterday",
                        zeroline = TRUE, zerolinecolor = ou_col$ink,
                        gridcolor = "#e6eae2"),
           yaxis = list(title = ""), margin = list(l = 70, t = 10),
           bargap = 0.35) |>
    ou_plotly()
}

swing_table <- function(rows_today) {
  wide <- rows_today |>
    select(Team, Record = `Current Record`, Player,
           change = `Change in Points Since Yesterday`) |>
    pivot_wider(names_from = Player, values_from = change)
  # Signed numbers only: blue and red are reserved for Over and Under
  players <- setdiff(names(wide), c("Team", "Record"))
  wide <- wide |> mutate(across(all_of(players), \(x) sprintf("%+d", x)))
  DT::datatable(wide, rownames = FALSE,
                options = list(dom = "t", pageLength = nrow(wide), ordering = FALSE,
                               columnDefs = list(list(className = "dt-right",
                                                      targets = seq_along(players) + 1))))
}
