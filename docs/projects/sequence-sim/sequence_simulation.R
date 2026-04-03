# =============================================================================
# Sequence Board Game Simulation - 3 Players, 3 Strategy Levels
# =============================================================================
#
# Strategy Levels:
#   1 = Novice   : Picks random playable card and random valid position.
#   2 = Middling : Greedy; extends own longest partial run each turn.
#   3 = Optimal  : Evaluates offense + defense + Jack management.
# =============================================================================

# --- Safe sampling helpers (avoids R's sample(n,1) trap) ---
safe_sample_idx <- function(n) {
  if (n <= 0) return(NULL)
  if (n == 1) return(1L)
  sample.int(n, 1L)
}

safe_sample_vec <- function(v) {
  n <- length(v)
  if (n == 0) return(NULL)
  if (n == 1) return(v[1])
  v[sample.int(n, 1L)]
}

pick_random_row <- function(mat) {
  nr <- nrow(mat)
  if (is.null(nr) || nr == 0) return(NULL)
  mat[safe_sample_idx(nr), , drop = FALSE]
}

# --- Official Board Layout ---
create_board <- function() {
  matrix(c(
    "FREE","2S","3S","4S","5S","6S","7S","8S","9S","FREE",
    "6C","5C","4C","3C","2C","AH","KH","QH","10H","10S",
    "7C","AS","2D","3D","4D","5D","6D","7D","9H","QS",
    "8C","KS","6C","5C","4C","3C","2C","8D","8H","KS",
    "9C","QS","7C","6H","5H","4H","AH","9D","7H","AS",
    "10C","10S","8C","7H","2H","3H","KH","10D","6H","2D",
    "QC","9S","9C","8H","9H","10H","QH","QD","5H","3D",
    "KC","8S","10C","QC","KC","AC","AD","KD","4H","4D",
    "AC","7S","6S","5S","4S","3S","2S","2H","3H","5D",
    "FREE","AD","KD","QD","10D","9D","8D","7D","6D","FREE"
  ), nrow = 10, ncol = 10, byrow = TRUE)
}

create_chips  <- function() {
  m <- matrix(0L, 10, 10)
  for (p in list(c(1,1),c(1,10),c(10,1),c(10,10))) m[p[1],p[2]] <- 9L
  m
}

create_locked <- function() matrix(FALSE, 10, 10)

create_deck <- function() {
  suits <- c("S","D","C","H")
  ranks <- c("2","3","4","5","6","7","8","9","10","J","Q","K","A")
  deck  <- paste0(rep(ranks, times = 4), rep(suits, each = 13))
  sample(rep(deck, 2))
}

# --- Helpers ---
is_two_eyed <- function(card) card %in% c("JD","JC")
is_one_eyed <- function(card) card %in% c("JS","JH")

is_dead <- function(card, board, chips) {
  if (is_two_eyed(card) || is_one_eyed(card)) return(FALSE)
  matches <- which(board == card, arr.ind = TRUE)
  if (nrow(matches) == 0) return(TRUE)
  all(chips[matches] != 0)
}

get_positions <- function(card, board, chips, locked) {
  if (is_two_eyed(card)) return(which(chips == 0, arr.ind = TRUE))
  if (is_one_eyed(card)) return(which(chips > 0 & chips < 9 & !locked, arr.ind = TRUE))
  which(board == card & chips == 0, arr.ind = TRUE)
}

# Filter one-eyed jack positions to only opponent chips
filter_opponent <- function(positions, chips, player) {
  if (nrow(positions) == 0) return(positions)
  keep <- apply(positions, 1, function(rc) chips[rc[1], rc[2]] != player)
  positions[keep, , drop = FALSE]
}

DIRS <- list(c(0,1), c(1,0), c(1,1), c(1,-1))

find_and_lock <- function(chips, locked, player) {
  found <- FALSE
  for (r in 1:10) for (cc in 1:10) for (d in DIRS) {
    cells <- list()
    for (s in 0:4) {
      rr <- r + d[1]*s; ccc <- cc + d[2]*s
      if (rr<1||rr>10||ccc<1||ccc>10) break
      v <- chips[rr, ccc]
      if (v == player || v == 9L) cells <- c(cells, list(c(rr, ccc)))
      else break
    }
    if (length(cells) >= 5) {
      found <- TRUE
      for (cl in cells) locked[cl[1], cl[2]] <- TRUE
    }
  }
  list(found = found, locked = locked)
}

# -------------------------------------------------------------------------
# Run length at (r,c) for a player: longest contiguous run in any direction
# -------------------------------------------------------------------------
run_length_at <- function(r, c, chips, player) {
  best <- 0
  for (d in DIRS) {
    run <- 1
    for (sign in c(1, -1)) for (step in 1:4) {
      rr <- r + d[1]*step*sign; cc <- c + d[2]*step*sign
      if (rr<1||rr>10||cc<1||cc>10) break
      v <- chips[rr, cc]
      if (v == player || v == 9L) run <- run + 1 else break
    }
    best <- max(best, run)
  }
  best
}

# -------------------------------------------------------------------------
# Count how many directions from (r,c) could form a 5-in-a-row for player
# -------------------------------------------------------------------------
potential_lines <- function(r, c, chips, player) {
  count <- 0
  for (d in DIRS) {
    for (start in -4:0) {
      friendly <- 0; blocked <- FALSE
      for (s in 0:4) {
        rr <- r + d[1]*(start+s); cc <- c + d[2]*(start+s)
        if (rr<1||rr>10||cc<1||cc>10) { blocked <- TRUE; break }
        v <- chips[rr, cc]
        if (v == player || v == 9L || v == 0L) {
          if (v == player || v == 9L) friendly <- friendly + 1
        } else { blocked <- TRUE; break }
      }
      if (!blocked && friendly >= 1) { count <- count + 1; break }
    }
  }
  count
}

# -------------------------------------------------------------------------
# Max run length any opponent has through (r,c)
# -------------------------------------------------------------------------
max_opponent_threat <- function(r, c, chips, player) {
  best <- 0
  for (opp in setdiff(1:3, player)) {
    best <- max(best, run_length_at(r, c, chips, opp))
  }
  best
}

# =========================================================================
#  STRATEGY FUNCTIONS
# =========================================================================

# --- Level 1: Novice (random) ---
novice_move <- function(hand, board, chips, locked, player) {
  # find all playable card indices
  playable <- c()
  for (ci in seq_along(hand)) {
    card <- hand[ci]
    pos <- get_positions(card, board, chips, locked)
    if (nrow(pos) == 0) next
    if (is_one_eyed(card)) {
      opp <- filter_opponent(pos, chips, player)
      if (nrow(opp) > 0) playable <- c(playable, ci)
    } else {
      playable <- c(playable, ci)
    }
  }
  if (length(playable) == 0) return(NULL)
  
  ci   <- safe_sample_vec(playable)
  card <- hand[ci]
  positions <- get_positions(card, board, chips, locked)
  if (is_one_eyed(card)) positions <- filter_opponent(positions, chips, player)
  if (nrow(positions) == 0) return(NULL)
  
  row <- pick_random_row(positions)
  list(ci = ci, card = card, pos = as.integer(row[1, ]))
}

# --- Level 2: Middling (greedy own-run) ---
middling_move <- function(hand, board, chips, locked, player) {
  for (ci in seq_along(hand)) {
    card <- hand[ci]
    positions <- get_positions(card, board, chips, locked)
    if (nrow(positions) == 0) next
    
    if (is_one_eyed(card)) {
      opp <- filter_opponent(positions, chips, player)
      if (nrow(opp) == 0) next
      best_t <- -1; best_p <- opp[1, ]
      for (i in seq_len(nrow(opp))) {
        owner <- chips[opp[i,1], opp[i,2]]
        t <- run_length_at(opp[i,1], opp[i,2], chips, owner)
        if (t > best_t) { best_t <- t; best_p <- opp[i, ] }
      }
      return(list(ci = ci, card = card, pos = as.integer(best_p)))
    }
    
    best_s <- -1; best_p <- positions[1, ]
    for (i in seq_len(nrow(positions))) {
      chips[positions[i,1], positions[i,2]] <- as.integer(player)
      s <- run_length_at(positions[i,1], positions[i,2], chips, player)
      chips[positions[i,1], positions[i,2]] <- 0L
      if (s > best_s) { best_s <- s; best_p <- positions[i, ] }
    }
    return(list(ci = ci, card = card, pos = as.integer(best_p)))
  }
  NULL
}

# --- Level 3: Near-Optimal (offense + defense + Jack management) ---
optimal_move <- function(hand, board, chips, locked, player) {
  best_score <- -Inf
  best_move  <- NULL
  
  for (ci in seq_along(hand)) {
    card <- hand[ci]
    positions <- get_positions(card, board, chips, locked)
    if (nrow(positions) == 0) next
    
    if (is_one_eyed(card)) {
      opp <- filter_opponent(positions, chips, player)
      if (nrow(opp) == 0) next
      for (i in seq_len(nrow(opp))) {
        owner  <- chips[opp[i,1], opp[i,2]]
        threat <- run_length_at(opp[i,1], opp[i,2], chips, owner)
        score  <- threat * 20
        if (threat >= 4) score <- score + 200
        if (score > best_score) {
          best_score <- score
          best_move  <- list(ci=ci, card=card, pos=as.integer(opp[i,]))
        }
      }
      next
    }
    
    if (is_two_eyed(card)) {
      other_playable <- FALSE
      for (j in seq_along(hand)) {
        if (j == ci) next
        if (nrow(get_positions(hand[j], board, chips, locked)) > 0) {
          other_playable <- TRUE; break
        }
      }
      jack_penalty <- if (other_playable) 15 else 0
    } else {
      jack_penalty <- 0
    }
    
    for (i in seq_len(nrow(positions))) {
      r <- positions[i, 1]; cc <- positions[i, 2]
      
      chips[r, cc] <- as.integer(player)
      own_run <- run_length_at(r, cc, chips, player)
      pot     <- potential_lines(r, cc, chips, player)
      chips[r, cc] <- 0L
      
      def <- max_opponent_threat(r, cc, chips, player)
      
      if (own_run >= 5) {
        score <- 10000
      } else {
        score <- own_run^3 * 5 + pot * 3 + def^2 * 8 - jack_penalty
        if (own_run == 4) score <- score + 150
        if (def >= 4) score <- score + 100
      }
      
      if (score > best_score) {
        best_score <- score
        best_move  <- list(ci=ci, card=card, pos=as.integer(positions[i,]))
      }
    }
  }
  best_move
}

# =========================================================================
#  GAME ENGINE
# =========================================================================

play_move <- function(move, hand, chips, board, discard_pile, deck, player) {
  pos <- move$pos
  if (is_one_eyed(move$card)) {
    chips[pos[1], pos[2]] <- 0L
  } else {
    chips[pos[1], pos[2]] <- as.integer(player)
  }
  
  discard_pile <- c(discard_pile, move$card)
  
  if (length(deck) == 0 && length(discard_pile) > 0) {
    deck <- sample(discard_pile); discard_pile <- character(0)
  }
  if (length(deck) > 0) {
    hand[move$ci] <- deck[1]; deck <- deck[-1]
  } else {
    hand <- hand[-move$ci]
  }
  
  list(hand = hand, chips = chips, deck = deck, discard_pile = discard_pile)
}

simulate_one_game <- function(strategies = c(3, 2, 1), verbose = FALSE) {
  board  <- create_board()
  chips  <- create_chips()
  locked <- create_locked()
  deck   <- create_deck()
  
  hands <- list(deck[1:7], deck[8:14], deck[15:21])
  deck  <- deck[22:length(deck)]
  discard_pile <- character(0)
  
  strategy_fn <- list(novice_move, middling_move, optimal_move)
  
  turn <- 0
  repeat {
    for (p in 1:3) {
      turn <- turn + 1
      if (turn > 600) return(NA_integer_)
      
      hand <- hands[[p]]
      if (length(hand) == 0) next
      
      # dead card discard (one per turn)
      for (ci in seq_along(hand)) {
        cd <- hand[ci]
        if (!is_two_eyed(cd) && !is_one_eyed(cd) && is_dead(cd, board, chips)) {
          discard_pile <- c(discard_pile, cd)
          if (length(deck) == 0 && length(discard_pile) > 0) {
            deck <- sample(discard_pile); discard_pile <- character(0)
          }
          if (length(deck) > 0) { hand[ci] <- deck[1]; deck <- deck[-1] }
          else hand <- hand[-ci]
          hands[[p]] <- hand
          break
        }
      }
      
      if (length(hand) == 0) next
      
      move <- strategy_fn[[strategies[p]]](hand, board, chips, locked, p)
      
      if (!is.null(move)) {
        result <- play_move(move, hand, chips, board, discard_pile, deck, p)
        hands[[p]]   <- result$hand
        chips        <- result$chips
        deck         <- result$deck
        discard_pile <- result$discard_pile
        
        res    <- find_and_lock(chips, locked, p)
        locked <- res$locked
        if (res$found) {
          if (verbose) cat(sprintf("Player %d (strategy %d) wins on turn %d!\n",
                                   p, strategies[p], turn))
          return(as.integer(p))
        }
      }
    }
  }
}

# =========================================================================
#  BATCH SIMULATION
# =========================================================================

run_batch <- function(n_games    = 1000,
                      strategies = c(3, 2, 1),
                      seed       = 42) {
  set.seed(seed)
  labels <- c("Novice", "Middling", "Optimal")
  
  cat("=============================================================\n")
  cat("  Sequence Batch Simulation\n")
  cat("=============================================================\n")
  cat(sprintf("  Games to play : %d\n", n_games))
  cat(sprintf("  Player 1 (Green) : %s (level %d)\n", labels[strategies[1]], strategies[1]))
  cat(sprintf("  Player 2 (Blue)  : %s (level %d)\n", labels[strategies[2]], strategies[2]))
  cat(sprintf("  Player 3 (Red)   : %s (level %d)\n", labels[strategies[3]], strategies[3]))
  cat("=============================================================\n\n")
  
  wins  <- integer(3)
  draws <- 0L
  pb    <- txtProgressBar(min = 0, max = n_games, style = 3)
  
  for (g in seq_len(n_games)) {
    winner <- tryCatch(
      simulate_one_game(strategies = strategies, verbose = FALSE),
      error = function(e) NA_integer_
    )
    if (is.na(winner)) draws <- draws + 1L
    else wins[winner] <- wins[winner] + 1L
    setTxtProgressBar(pb, g)
  }
  close(pb)
  
  cat("\n\n====================== RESULTS ======================\n")
  for (p in 1:3) {
    pct <- round(100 * wins[p] / n_games, 1)
    bar <- paste(rep("#", round(pct / 2)), collapse = "")
    cat(sprintf("  Player %d %-8s : %4d wins (%5.1f%%) %s\n",
                p, paste0("(", labels[strategies[p]], ")"),
                wins[p], pct, bar))
  }
  if (draws > 0) cat(sprintf("  Draws/errors     : %4d\n", draws))
  cat("=====================================================\n")
  
  invisible(list(wins = wins, draws = draws, strategies = strategies,
                 labels = labels[strategies]))
}

# =========================================================================
#  RUN IT
# =========================================================================

# Player 1 = Optimal, Player 2 = Middling, Player 3 = Novice
result <- run_batch(n_games = 1000, strategies = c(3, 2, 1))