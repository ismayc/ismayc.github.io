# 06b-validate-seeding.R
# ============================================================================
# Compare our tiebreaker-computed seedings against the official source
# (NBA.com or ESPN standings with seeds applied).
#
# Run AFTER 05-playoff-seeding.R has produced `seeded_standings`.
# Also requires: `standings`, `scores_tidy`, `meta` from earlier pipeline.
#
# Steps:
#   1. Run 06a-fetch-official-standings.py to get official seeds
#   2. Compare seed-by-seed against our computed seedings
#   3. Report any discrepancies
# ============================================================================

library(tidyverse)
library(glue)

# --------------------------------------------------------------------------
# Step 1: Fetch official standings
# --------------------------------------------------------------------------

cat("\n================================================================\n")
cat("STEP 1: Fetching official standings from NBA.com / ESPN\n")
cat("================================================================\n\n")

# Run the Python fetcher as a subprocess (avoids sys.exit crashing R)
py_result <- system2(
  command = Sys.which("python3"),
  args = "06a-fetch-official-standings.py",
  stdout = TRUE,
  stderr = TRUE
)
cat(paste(py_result, collapse = "\n"), "\n")

if (!file.exists("official_standings.csv")) {
  # Try with reticulate's Python as fallback
  cat("\nDirect python3 failed. Trying reticulate Python...\n")
  py_path <- reticulate::py_config()$python
  py_result2 <- system2(
    command = py_path,
    args = "06a-fetch-official-standings.py",
    stdout = TRUE,
    stderr = TRUE
  )
  cat(paste(py_result2, collapse = "\n"), "\n")
}

if (!file.exists("official_standings.csv")) {
  stop("Failed to fetch official standings. Check 06a output above.")
}

official <- read_csv("official_standings.csv", show_col_types = FALSE) %>%
  select(seed_official = seed,
         team_name,
         conference,
         wins_official = wins,
         losses_official = losses,
         source)

# --------------------------------------------------------------------------
# Step 2: Prepare our computed standings
# --------------------------------------------------------------------------

cat("\n================================================================\n")
cat("STEP 2: Preparing computed standings from 05-playoff-seeding.R\n")
cat("================================================================\n\n")

# `seeded_standings` should already exist from compute_playoff_seeding()
if (!exists("seeded_standings")) {
  cat("seeded_standings not found. Sourcing pipeline...\n")
  source("05-playoff-seeding.R")
  seeded_standings <- compute_playoff_seeding(standings, scores_tidy, meta)
}

computed <- seeded_standings %>%
  select(seed_computed = seed,
         team_name,
         conference,
         wins_computed = wins,
         losses_computed = losses)

# --------------------------------------------------------------------------
# Step 3: Compare
# --------------------------------------------------------------------------

cat("\n================================================================\n")
cat("STEP 3: Comparing official vs. computed seedings\n")
cat("================================================================\n\n")

comparison <- official %>%
  inner_join(computed, by = c("team_name", "conference")) %>%
  mutate(
    seed_match  = seed_official == seed_computed,
    wins_match  = wins_official == wins_computed,
    losses_match = losses_official == losses_computed
  ) %>%
  arrange(conference, seed_official)

# Check join quality
n_joined <- nrow(comparison)
if (n_joined < 30) {
  cat(glue("WARNING: Only {n_joined}/30 teams matched by name + conference.\n"))
  
  # Show what didn't match
  unmatched_official <- official %>%
    anti_join(computed, by = c("team_name", "conference"))
  unmatched_computed <- computed %>%
    anti_join(official, by = c("team_name", "conference"))
  
  if (nrow(unmatched_official) > 0) {
    cat("\nUnmatched in official source:\n")
    print(unmatched_official %>% select(team_name, conference))
  }
  if (nrow(unmatched_computed) > 0) {
    cat("\nUnmatched in computed:\n")
    print(unmatched_computed %>% select(team_name, conference))
  }
  cat("\n")
}

# --- Report W-L discrepancies first (data freshness issue) ---
wl_mismatches <- comparison %>%
  filter(!wins_match | !losses_match)

if (nrow(wl_mismatches) > 0) {
  cat("WARNING: Win-loss record mismatches detected.\n")
  cat("This likely means your game data (current_year.csv) is not\n")
  cat("up to date with the official source. Seed comparison may be\n")
  cat("unreliable for teams with different records.\n\n")
  wl_mismatches %>%
    select(team_name, conference,
           wins_official, wins_computed,
           losses_official, losses_computed) %>%
    print(n = Inf)
  cat("\n")
} else {
  cat("All W-L records match between official source and computed data.\n\n")
}

# --- Report seed discrepancies ---
seed_mismatches <- comparison %>%
  filter(!seed_match)

if (nrow(seed_mismatches) > 0) {
  cat(glue("SEED MISMATCHES: {nrow(seed_mismatches)} team(s) have different seeds\n\n"))
  seed_mismatches %>%
    select(team_name, conference,
           seed_official, seed_computed,
           wins_official, losses_official) %>%
    print(n = Inf)
  cat("\n")
  
  # Break down by whether the W-L records also differ
  seed_only <- seed_mismatches %>% filter(wins_match & losses_match)
  if (nrow(seed_only) > 0) {
    cat("Of these, the following have matching W-L but different seeds\n")
    cat("(true tiebreaker discrepancy):\n\n")
    seed_only %>%
      select(team_name, conference, seed_official, seed_computed,
             wins_official, losses_official) %>%
      print(n = Inf)
  }
  
} else {
  cat("ALL SEEDS MATCH! Computed seedings are identical to official source.\n")
}

# --- Print side-by-side for visual inspection ---
cat("\n\n================================================================\n")
cat("FULL COMPARISON (side by side)\n")
cat("================================================================\n")

for (conf in c("East", "West")) {
  cat(glue("\n--- {conf}ern Conference ---\n\n"))
  cat(sprintf("%-3s  %-30s  %3s  %-4s  %-5s  %s\n",
              "Seed", "Team", "Off", "Comp", "W-L", "Match?"))
  cat(paste(rep("-", 65), collapse = ""), "\n")
  
  conf_rows <- comparison %>%
    filter(conference == conf) %>%
    arrange(seed_official)
  
  for (i in seq_len(nrow(conf_rows))) {
    row <- conf_rows[i, ]
    match_str <- if_else(row$seed_match, "  OK", " DIFF")
    cat(sprintf("%2d.  %-30s  %3d  %3d   %d-%-2d  %s\n",
                row$seed_official,
                row$team_name,
                row$seed_official,
                row$seed_computed,
                row$wins_official,
                row$losses_official,
                match_str))
    if (row$seed_official == 6) {
      cat(paste(rep("=", 65), collapse = ""), "  PLAYOFF CUTOFF\n")
    }
    if (row$seed_official == 10) {
      cat(paste(rep("=", 65), collapse = ""), "  PLAY-IN CUTOFF\n")
    }
  }
}

# --- Summary ---
n_total    <- nrow(comparison)
n_matching <- sum(comparison$seed_match)
n_wl_ok    <- sum(comparison$wins_match & comparison$losses_match)

cat(glue("\n\nSUMMARY: {n_matching}/{n_total} seeds match. "),
    glue("{n_wl_ok}/{n_total} W-L records match.\n"))
cat(glue("\nSource: {unique(comparison$source)}\n"))
cat(glue("\nValidated at: {Sys.time()}\n"))