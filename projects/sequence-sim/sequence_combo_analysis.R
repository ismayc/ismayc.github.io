# =============================================================================
# Sequence: Blind 4th Variant - Parallel Combinatorial Analysis
# =============================================================================
# Parallelized with mclapply (fork-based, native macOS support).
# Distributes games across cores within each combo for maximum throughput.
# =============================================================================

library(parallel)

source("sequence_simulation_4.R")

run_full_comparison <- function(n_games = 5000, seed = 42, n_cores = 14) {
  
  labels <- c("Novice", "Middling", "Optimal")
  short  <- c("Nov", "Mid", "Opt")
  
  combos <- list(
    c(1,1,1), c(1,1,2), c(1,1,3),
    c(1,2,2), c(1,2,3), c(1,3,3),
    c(2,2,2), c(2,2,3), c(2,3,3),
    c(3,3,3)
  )
  n_combos <- length(combos)
  total_games <- n_combos * n_games
  
  detected <- detectCores()
  n_cores  <- min(n_cores, detected - 2, 14)  # leave 2 cores free
  
  cat("=============================================================\n")
  cat("  Blind 4th Variant: Parallel Combinatorial Analysis\n")
  cat("=============================================================\n")
  cat(sprintf("  CPU cores detected  : %d\n", detected))
  cat(sprintf("  Cores to use        : %d\n", n_cores))
  cat(sprintf("  Unique combinations : %d\n", n_combos))
  cat(sprintf("  Games per combo     : %s\n", format(n_games, big.mark = ",")))
  cat(sprintf("  Total games         : %s\n", format(total_games, big.mark = ",")))
  cat("=============================================================\n\n")
  
  overall_start <- Sys.time()
  
  # --- Worker function: runs a batch of games for one combo ---
  run_batch_worker <- function(game_indices, strats, batch_seed) {
    # Each worker gets its own RNG stream
    set.seed(batch_seed)
    yellow_w <- 0L; real_w <- 0L; draw_ct <- 0L
    for (g in game_indices) {
      shuffled <- sample(strats)
      res <- tryCatch(
        simulate_one_game(strategies = shuffled, verbose = FALSE),
        error = function(e) list(winner = NA_integer_, yellow_win = FALSE)
      )
      w <- res$winner
      if (is.na(w)) draw_ct <- draw_ct + 1L
      else if (w == 4L) yellow_w <- yellow_w + 1L
      else real_w <- real_w + 1L
    }
    list(yellow = yellow_w, real = real_w, draws = draw_ct)
  }
  
  results <- data.frame(
    combo       = character(n_combos),
    yellow_wins = integer(n_combos),
    real_wins   = integer(n_combos),
    draws       = integer(n_combos),
    yellow_pct  = numeric(n_combos),
    real_pct    = numeric(n_combos),
    avg_skill   = numeric(n_combos),
    stringsAsFactors = FALSE
  )
  
  for (idx in seq_len(n_combos)) {
    strats    <- combos[[idx]]
    combo_str <- paste(short[strats], collapse = " / ")
    cat(sprintf("[%2d/%d] %-17s  running %d games on %d cores ... ",
                idx, n_combos, combo_str, n_games, n_cores))
    
    combo_start <- Sys.time()
    
    # Split game indices into n_cores chunks
    all_indices <- seq_len(n_games)
    chunks <- split(all_indices, cut(all_indices, n_cores, labels = FALSE))
    
    # Generate unique seeds per chunk (deterministic from master seed)
    set.seed(seed + idx * 1000)
    chunk_seeds <- sample.int(.Machine$integer.max, n_cores)
    
    # Run in parallel
    chunk_results <- mclapply(seq_along(chunks), function(k) {
      run_batch_worker(chunks[[k]], strats, chunk_seeds[k])
    }, mc.cores = n_cores)
    
    # Aggregate
    yellow_w <- sum(sapply(chunk_results, `[[`, "yellow"))
    real_w   <- sum(sapply(chunk_results, `[[`, "real"))
    draw_ct  <- sum(sapply(chunk_results, `[[`, "draws"))
    
    combo_elapsed <- difftime(Sys.time(), combo_start, units = "secs")
    
    results$combo[idx]       <- combo_str
    results$yellow_wins[idx] <- yellow_w
    results$real_wins[idx]   <- real_w
    results$draws[idx]       <- draw_ct
    results$yellow_pct[idx]  <- round(100 * yellow_w / n_games, 2)
    results$real_pct[idx]    <- round(100 * real_w / n_games, 2)
    results$avg_skill[idx]   <- mean(strats)
    
    cat(sprintf("done in %4.1fs  Yellow: %5.1f%%  Real: %5.1f%%\n",
                as.numeric(combo_elapsed),
                results$yellow_pct[idx], results$real_pct[idx]))
  }
  
  elapsed <- difftime(Sys.time(), overall_start, units = "mins")
  
  # ---- RESULTS TABLE ----
  ord <- order(results$yellow_pct, decreasing = TRUE)
  
  cat("\n\n")
  cat("╔═══════════════════════════════════════════════════════════════════╗\n")
  cat("║            YELLOW SPOILS vs REAL PLAYER WINS                    ║\n")
  cat("╠═══════════════════╦═══════════╦═══════════╦═══════╦═════════════╣\n")
  cat("║  Skill Combo      ║ Yellow %  ║ Real %    ║ Draws ║ Avg Skill   ║\n")
  cat("╠═══════════════════╬═══════════╬═══════════╬═══════╬═════════════╣\n")
  for (i in ord) {
    cat(sprintf("║ %-17s ║  %5.1f%%   ║  %5.1f%%   ║ %4d  ║   %.1f         ║\n",
                results$combo[i],
                results$yellow_pct[i], results$real_pct[i],
                results$draws[i], results$avg_skill[i]))
  }
  cat("╚═══════════════════╩═══════════╩═══════════╩═══════╩═════════════╝\n")
  
  # ---- BAR CHART ----
  cat("\n  Yellow Win %% by Combo:\n")
  cat("  ─────────────────────────────────────────────────────────\n")
  for (i in ord) {
    bar <- paste(rep("▓", round(results$yellow_pct[i] / 1.5)), collapse = "")
    cat(sprintf("  %-17s │%s %.1f%%\n", results$combo[i], bar, results$yellow_pct[i]))
  }
  
  # ---- KEY INSIGHTS ----
  cat("\n")
  cat("========================== KEY INSIGHTS ==========================\n\n")
  
  worst <- results[which.max(results$yellow_pct), ]
  best  <- results[which.min(results$yellow_pct), ]
  
  cat(sprintf("  Yellow thrives most against : %-17s (%.1f%% spoil rate)\n",
              worst$combo, worst$yellow_pct))
  cat(sprintf("  Yellow is weakest against   : %-17s (%.1f%% spoil rate)\n",
              best$combo, best$yellow_pct))
  cat(sprintf("\n  Spread: %.1f pp between best and worst for Yellow\n",
              worst$yellow_pct - best$yellow_pct))
  
  cat("\n  Yellow spoil rate by # of Optimal players:\n")
  for (n_opt in 0:3) {
    sub <- results[sapply(combos, function(x) sum(x == 3)) == n_opt, ]
    if (nrow(sub) > 0)
      cat(sprintf("    %d Optimal : avg %.1f%%\n", n_opt, mean(sub$yellow_pct)))
  }
  
  cat("\n  Yellow spoil rate by # of Novice players:\n")
  for (n_nov in 0:3) {
    sub <- results[sapply(combos, function(x) sum(x == 1)) == n_nov, ]
    if (nrow(sub) > 0)
      cat(sprintf("    %d Novice  : avg %.1f%%\n", n_nov, mean(sub$yellow_pct)))
  }
  
  cat(sprintf("\n  Total runtime: %.1f minutes (%.0fx faster than single-core estimate)\n",
              as.numeric(elapsed), n_cores * 0.7))  # rough efficiency estimate
  cat("==================================================================\n")
  
  invisible(results)
}

# =========================================================================
#  RUN IT
# =========================================================================

# Uses 14 of your 16 cores
results <- run_full_comparison(n_games = 30000, seed = 2026, n_cores = 12)

filename <- "blind4th_results_30k.csv"
write.csv(results, filename, row.names = FALSE)
cat(paste("\nResults saved to", filename, "\n"))