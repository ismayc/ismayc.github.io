# 05-playoff-seeding.R
# ============================================================================
# NBA Playoff Tiebreaker Procedures
# Based on: https://ak-static-int.nba.com/wp-content/uploads/sites/2/2017/06/NBA_Tiebreaker_Procedures.pdf
#
# Required objects (from earlier pipeline scripts):
#   - standings:    team_name, conference, division, wins, losses, `Winning Pct`, abbreviation
#   - scores_tidy:  team_slug, opponent_slug, score, opponent_score (one row per team per game)
#   - meta:         team, conference, division, abbreviation
# ============================================================================

library(dplyr)
library(stringr)

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

#' Compute W-L record for one team against a set of opponents
record_vs <- function(team_abbr, opponent_abbrs, scores_tidy) {
  games <- scores_tidy %>%
    filter(team_slug == team_abbr, opponent_slug %in% opponent_abbrs)
  wins   <- sum(games$score > games$opponent_score, na.rm = TRUE)
  losses <- sum(games$score < games$opponent_score, na.rm = TRUE)
  total  <- wins + losses
  pct    <- if (total > 0) wins / total else NA_real_
  tibble(abbreviation = team_abbr, wins = wins, losses = losses, pct = pct)
}

#' Compute net point differential for one team (all games)
point_differential <- function(team_abbr, scores_tidy) {
  games <- scores_tidy %>% filter(team_slug == team_abbr)
  sum(games$score - games$opponent_score, na.rm = TRUE)
}

#' Identify the best team in each division (by Winning Pct) within a conference
#' Returns a character vector of abbreviations that lead their division.
#' If two teams tie for a division lead, the 2-team tiebreaker is applied
#' *only* to determine the division winner (per rule 1a/1b).
get_division_leaders <- function(conf_standings, scores_tidy, meta) {
  leaders <- character(0)
  
  divisions <- unique(conf_standings$division)
  for (div in divisions) {
    div_teams <- conf_standings %>% filter(division == div)
    max_pct   <- max(div_teams$`Winning Pct`, na.rm = TRUE)
    top_teams <- div_teams %>% filter(`Winning Pct` == max_pct)
    
    if (nrow(top_teams) == 1) {
      leaders <- c(leaders, top_teams$abbreviation)
    } else {
      # Break tie among division co-leaders using the 2-team procedure
      # but only to pick the single division winner (rule 1b)
      abbrs <- top_teams$abbreviation
      if (length(abbrs) == 2) {
        ordered <- break_two_team_tie(
          abbrs[1], abbrs[2],
          conf_standings, scores_tidy, meta,
          division_leaders = character(0)  # no leaders determined yet for this step
        )
        leaders <- c(leaders, ordered[1])
      } else {
        # 3+ way tie for division lead -- use multi-team procedure
        ordered <- break_multi_team_tie(
          abbrs, conf_standings, scores_tidy, meta,
          division_leaders = character(0)
        )
        leaders <- c(leaders, ordered[1])
      }
    }
  }
  leaders
}

# ---------------------------------------------------------------------------
# Two-team tiebreaker (returns c(winner_abbr, loser_abbr))
# ---------------------------------------------------------------------------
break_two_team_tie <- function(t1, t2, standings_df, scores_tidy, meta,
                               division_leaders = character(0),
                               playoff_eligible_own = NULL,
                               playoff_eligible_other = NULL) {
  
  t1_info <- meta %>% filter(abbreviation == t1)
  t2_info <- meta %>% filter(abbreviation == t2)
  
  # (1) Head-to-head
  h2h <- scores_tidy %>%
    filter(team_slug == t1, opponent_slug == t2)
  t1_w <- sum(h2h$score > h2h$opponent_score, na.rm = TRUE)
  t1_l <- sum(h2h$score < h2h$opponent_score, na.rm = TRUE)
  if (t1_w != t1_l) {
    return(if (t1_w > t1_l) c(t1, t2) else c(t2, t1))
  }
  
  # (2) Division leader wins tie over non-leader
  t1_leader <- t1 %in% division_leaders
  t2_leader <- t2 %in% division_leaders
  if (t1_leader && !t2_leader) return(c(t1, t2))
  if (t2_leader && !t1_leader) return(c(t2, t1))
  
  # (3) Division record (only if same division)
  if (t1_info$division == t2_info$division) {
    div_opps <- meta %>%
      filter(division == t1_info$division) %>%
      pull(abbreviation)
    r1 <- record_vs(t1, div_opps, scores_tidy)
    r2 <- record_vs(t2, div_opps, scores_tidy)
    if (!is.na(r1$pct) && !is.na(r2$pct) && r1$pct != r2$pct) {
      return(if (r1$pct > r2$pct) c(t1, t2) else c(t2, t1))
    }
  }
  
  # (4) Conference record
  conf_opps <- meta %>%
    filter(conference == t1_info$conference) %>%
    pull(abbreviation)
  r1 <- record_vs(t1, conf_opps, scores_tidy)
  r2 <- record_vs(t2, conf_opps, scores_tidy)
  if (!is.na(r1$pct) && !is.na(r2$pct) && r1$pct != r2$pct) {
    return(if (r1$pct > r2$pct) c(t1, t2) else c(t2, t1))
  }
  
  # (5) Record vs playoff-eligible teams in own conference
  if (!is.null(playoff_eligible_own) && length(playoff_eligible_own) > 0) {
    r1 <- record_vs(t1, playoff_eligible_own, scores_tidy)
    r2 <- record_vs(t2, playoff_eligible_own, scores_tidy)
    if (!is.na(r1$pct) && !is.na(r2$pct) && r1$pct != r2$pct) {
      return(if (r1$pct > r2$pct) c(t1, t2) else c(t2, t1))
    }
  }
  
  # (6) Record vs playoff-eligible teams in other conference
  if (!is.null(playoff_eligible_other) && length(playoff_eligible_other) > 0) {
    r1 <- record_vs(t1, playoff_eligible_other, scores_tidy)
    r2 <- record_vs(t2, playoff_eligible_other, scores_tidy)
    if (!is.na(r1$pct) && !is.na(r2$pct) && r1$pct != r2$pct) {
      return(if (r1$pct > r2$pct) c(t1, t2) else c(t2, t1))
    }
  }
  
  # (7) Point differential
  pd1 <- point_differential(t1, scores_tidy)
  pd2 <- point_differential(t2, scores_tidy)
  if (pd1 != pd2) {
    return(if (pd1 > pd2) c(t1, t2) else c(t2, t1))
  }
  
  # Unresolvable -- fallback to alphabetical (deterministic stand-in for random draw)
  sort(c(t1, t2))
}

# ---------------------------------------------------------------------------
# Multi-team (3+) tiebreaker (returns ordered vector of abbreviations)
# ---------------------------------------------------------------------------
break_multi_team_tie <- function(team_abbrs, standings_df, scores_tidy, meta,
                                 division_leaders = character(0),
                                 playoff_eligible_own = NULL,
                                 playoff_eligible_other = NULL) {
  
  if (length(team_abbrs) <= 1) return(team_abbrs)
  if (length(team_abbrs) == 2) {
    return(break_two_team_tie(
      team_abbrs[1], team_abbrs[2],
      standings_df, scores_tidy, meta,
      division_leaders, playoff_eligible_own, playoff_eligible_other
    ))
  }
  
  remaining <- team_abbrs
  ordered   <- character(0)
  
  # Determine conference (all tied teams should be same conference)
  team_conf <- meta %>%
    filter(abbreviation == remaining[1]) %>%
    pull(conference)
  
  # --- Criterion 1: Division leader wins over non-leaders ---
  leaders     <- intersect(remaining, division_leaders)
  non_leaders <- setdiff(remaining, division_leaders)
  if (length(leaders) > 0 && length(non_leaders) > 0) {
    # Leaders get seeded above non-leaders.
    # If multiple leaders remain tied, continue with them.
    # Non-leaders continue separately.
    leader_order <- break_multi_team_tie(
      leaders, standings_df, scores_tidy, meta,
      division_leaders, playoff_eligible_own, playoff_eligible_other
    )
    non_leader_order <- break_multi_team_tie(
      non_leaders, standings_df, scores_tidy, meta,
      division_leaders, playoff_eligible_own, playoff_eligible_other
    )
    return(c(leader_order, non_leader_order))
  }
  
  # --- Criterion 2: Record among ALL tied teams ---
  records <- bind_rows(lapply(remaining, function(tm) {
    record_vs(tm, remaining, scores_tidy)
  }))
  
  if (n_distinct(records$pct, na.rm = TRUE) > 1) {
    # Some differentiation exists. Assign best team(s) their seed,
    # then restart with the rest (rule 1c).
    max_pct <- max(records$pct, na.rm = TRUE)
    best    <- records %>% filter(pct == max_pct) %>% pull(abbreviation)
    rest    <- setdiff(remaining, best)
    
    if (length(best) == 1) {
      ordered <- c(best, break_multi_team_tie(
        rest, standings_df, scores_tidy, meta,
        division_leaders, playoff_eligible_own, playoff_eligible_other
      ))
      return(ordered)
    } else {
      # Multiple teams still tied at top -- recurse on them separately
      best_ordered <- break_multi_team_tie(
        best, standings_df, scores_tidy, meta,
        division_leaders, playoff_eligible_own, playoff_eligible_other
      )
      rest_ordered <- break_multi_team_tie(
        rest, standings_df, scores_tidy, meta,
        division_leaders, playoff_eligible_own, playoff_eligible_other
      )
      return(c(best_ordered, rest_ordered))
    }
  }
  
  # --- Criterion 3: Division record (only if ALL teams are in the same division) ---
  team_divs <- meta %>%
    filter(abbreviation %in% remaining) %>%
    pull(division) %>%
    unique()
  
  if (length(team_divs) == 1) {
    div_opps <- meta %>%
      filter(division == team_divs) %>%
      pull(abbreviation)
    
    div_records <- bind_rows(lapply(remaining, function(tm) {
      record_vs(tm, div_opps, scores_tidy)
    }))
    
    if (n_distinct(div_records$pct, na.rm = TRUE) > 1) {
      max_pct <- max(div_records$pct, na.rm = TRUE)
      best    <- div_records %>% filter(pct == max_pct) %>% pull(abbreviation)
      rest    <- setdiff(remaining, best)
      
      best_ordered <- break_multi_team_tie(
        best, standings_df, scores_tidy, meta,
        division_leaders, playoff_eligible_own, playoff_eligible_other
      )
      rest_ordered <- break_multi_team_tie(
        rest, standings_df, scores_tidy, meta,
        division_leaders, playoff_eligible_own, playoff_eligible_other
      )
      return(c(best_ordered, rest_ordered))
    }
  }
  
  # --- Criterion 4: Conference record ---
  conf_opps <- meta %>%
    filter(conference == team_conf) %>%
    pull(abbreviation)
  
  conf_records <- bind_rows(lapply(remaining, function(tm) {
    record_vs(tm, conf_opps, scores_tidy)
  }))
  
  if (n_distinct(conf_records$pct, na.rm = TRUE) > 1) {
    max_pct <- max(conf_records$pct, na.rm = TRUE)
    best    <- conf_records %>% filter(pct == max_pct) %>% pull(abbreviation)
    rest    <- setdiff(remaining, best)
    
    best_ordered <- break_multi_team_tie(
      best, standings_df, scores_tidy, meta,
      division_leaders, playoff_eligible_own, playoff_eligible_other
    )
    rest_ordered <- break_multi_team_tie(
      rest, standings_df, scores_tidy, meta,
      division_leaders, playoff_eligible_own, playoff_eligible_other
    )
    return(c(best_ordered, rest_ordered))
  }
  
  # --- Criterion 5: Record vs playoff-eligible teams in own conference ---
  if (!is.null(playoff_eligible_own) && length(playoff_eligible_own) > 0) {
    po_records <- bind_rows(lapply(remaining, function(tm) {
      record_vs(tm, playoff_eligible_own, scores_tidy)
    }))
    
    if (n_distinct(po_records$pct, na.rm = TRUE) > 1) {
      max_pct <- max(po_records$pct, na.rm = TRUE)
      best    <- po_records %>% filter(pct == max_pct) %>% pull(abbreviation)
      rest    <- setdiff(remaining, best)
      
      best_ordered <- break_multi_team_tie(
        best, standings_df, scores_tidy, meta,
        division_leaders, playoff_eligible_own, playoff_eligible_other
      )
      rest_ordered <- break_multi_team_tie(
        rest, standings_df, scores_tidy, meta,
        division_leaders, playoff_eligible_own, playoff_eligible_other
      )
      return(c(best_ordered, rest_ordered))
    }
  }
  
  # --- Criterion 6: Point differential ---
  pds <- tibble(
    abbreviation = remaining,
    pd = sapply(remaining, function(tm) point_differential(tm, scores_tidy))
  )
  
  if (n_distinct(pds$pd) > 1) {
    pds <- pds %>% arrange(desc(pd))
    max_pd <- max(pds$pd)
    best   <- pds %>% filter(pd == max_pd) %>% pull(abbreviation)
    rest   <- setdiff(remaining, best)
    
    best_ordered <- break_multi_team_tie(
      best, standings_df, scores_tidy, meta,
      division_leaders, playoff_eligible_own, playoff_eligible_other
    )
    rest_ordered <- break_multi_team_tie(
      rest, standings_df, scores_tidy, meta,
      division_leaders, playoff_eligible_own, playoff_eligible_other
    )
    return(c(best_ordered, rest_ordered))
  }
  
  # All criteria exhausted -- alphabetical fallback for random draw
  sort(remaining)
}

# ---------------------------------------------------------------------------
# Main entry point: seed one conference
# ---------------------------------------------------------------------------

#' Produce playoff seeding for a single conference.
#'
#' @param conf       "East" or "West"
#' @param standings  Full standings dataframe (all 30 teams)
#' @param scores_tidy  Tidy game results (one row per team per game)
#' @param meta       Team metadata with conference, division, abbreviation
#'
#' @return A tibble with columns: seed, team_name, conference, division,
#'         wins, losses, `Winning Pct`, abbreviation, plus a `seed_note`
#'         column indicating playoff status.
seed_conference <- function(conf, standings, scores_tidy, meta) {
  
  conf_standings <- standings %>%
    filter(conference == conf) %>%
    arrange(desc(`Winning Pct`))
  
  # Step 1: Determine division leaders (rule 1a -- break div ties first)
  division_leaders <- get_division_leaders(conf_standings, scores_tidy, meta)
  
  # Step 2: Determine playoff-eligible teams (top 10 by Winning Pct, including ties)
  # This is needed for tiebreaker criteria 5 and 6.
  # Use a preliminary cutoff: the 10th-best Winning Pct in the conference
  sorted_pcts <- sort(conf_standings$`Winning Pct`, decreasing = TRUE)
  cutoff_pct  <- if (length(sorted_pcts) >= 10) sorted_pcts[10] else min(sorted_pcts)
  
  playoff_eligible_own <- conf_standings %>%
    filter(`Winning Pct` >= cutoff_pct) %>%
    pull(abbreviation)
  
  # Other conference's playoff-eligible teams
  other_conf <- if (conf == "East") "West" else "East"
  other_standings <- standings %>%
    filter(conference == other_conf) %>%
    arrange(desc(`Winning Pct`))
  other_pcts   <- sort(other_standings$`Winning Pct`, decreasing = TRUE)
  other_cutoff <- if (length(other_pcts) >= 10) other_pcts[10] else min(other_pcts)
  
  playoff_eligible_other <- other_standings %>%
    filter(`Winning Pct` >= other_cutoff) %>%
    pull(abbreviation)
  
  # Step 3: Group teams by Winning Pct and break ties within each group
  pct_groups <- conf_standings %>%
    group_by(`Winning Pct`) %>%
    summarise(teams = list(abbreviation), .groups = "drop") %>%
    arrange(desc(`Winning Pct`))
  
  final_order <- character(0)
  
  for (i in seq_len(nrow(pct_groups))) {
    group_teams <- pct_groups$teams[[i]]
    
    if (length(group_teams) == 1) {
      final_order <- c(final_order, group_teams)
    } else if (length(group_teams) == 2) {
      ordered <- break_two_team_tie(
        group_teams[1], group_teams[2],
        conf_standings, scores_tidy, meta,
        division_leaders, playoff_eligible_own, playoff_eligible_other
      )
      final_order <- c(final_order, ordered)
    } else {
      ordered <- break_multi_team_tie(
        group_teams, conf_standings, scores_tidy, meta,
        division_leaders, playoff_eligible_own, playoff_eligible_other
      )
      final_order <- c(final_order, ordered)
    }
  }
  
  # Step 4: Build the seeded output
  result <- tibble(seed = seq_along(final_order), abbreviation = final_order) %>%
    inner_join(conf_standings, by = "abbreviation") %>%
    mutate(seed_note = case_when(
      seed <= 6  ~ "Playoff",
      seed <= 10 ~ "Play-In",
      TRUE       ~ ""
    )) %>%
    select(seed, team_name, conference, division, wins, losses,
           `Winning Pct`, `Games Back`, abbreviation, seed_note)
  
  result
}

# ---------------------------------------------------------------------------
# Top-level function: seed both conferences
# ---------------------------------------------------------------------------

#' Produce full NBA playoff seeding for both conferences.
#'
#' @inheritParams seed_conference
#' @return A tibble with all 30 teams, seeded 1-15 within each conference.
compute_playoff_seeding <- function(standings, scores_tidy, meta) {
  east <- seed_conference("East", standings, scores_tidy, meta)
  west <- seed_conference("West", standings, scores_tidy, meta)
  bind_rows(east, west)
}
