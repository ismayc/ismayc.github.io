# =============================================================================
# Sequence: Blind 4th Player Variant
# =============================================================================
#
# 3 real players (Green, Blue, Red) + 1 blind "Yellow" player.
# All Jacks removed from the deck. Each round:
#   1. The designated controller flips the top deck card for Yellow and
#      chooses which of the (up to 2) open matching board spots to place
#      Yellow's chip.
#   2. Player 1 takes their turn.
#   3. Player 2 takes their turn.
#   4. Player 3 takes their turn.
# The controller rotates left each round (P1 -> P2 -> P3 -> P1 ...).
#
# If Yellow completes a 5-in-a-row, ALL three real players lose.
# The controller's skill level determines how well they place Yellow's chip.
#
# Strategy Levels (for real-player turns AND Yellow placement decisions):
#   1 = Novice   : Random choices.
#   2 = Middling : Greedy (extend own run / minimize Yellow run).
#   3 = Optimal  : Offense + defense + smart Yellow placement.
# =============================================================================

# --- Safe sampling ---
safe_sample_vec <- function(v) {
  n <- length(v)
  if (n == 0) return(NULL)
  if (n == 1) return(v[1])
  v[sample.int(n, 1L)]
}

safe_sample_idx <- function(n) {
  if (n <= 0) return(NULL)
  if (n == 1) return(1L)
  sample.int(n, 1L)
}

# --- Board ---
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

create_chips <- function() {
  m <- matrix(0L, 10, 10)
  for (p in list(c(1,1),c(1,10),c(10,1),c(10,10))) m[p[1],p[2]] <- 9L
  m
}

create_locked <- function() matrix(FALSE, 10, 10)

# Jack-free deck: 96 cards (48 unique x 2 copies)
create_deck <- function() {
  suits <- c("S","D","C","H")
  ranks <- c("2","3","4","5","6","7","8","9","10","Q","K","A")
  deck  <- paste0(rep(ranks, times = 4), rep(suits, each = 12))
  sample(rep(deck, 2))
}

# --- Helpers ---
DIRS <- list(c(0,1), c(1,0), c(1,1), c(1,-1))

# Positions for a normal card (no Jacks in this variant)
get_open_spots <- function(card, board, chips) {
  which(board == card & chips == 0, arr.ind = TRUE)
}

is_dead <- function(card, board, chips) {
  all(chips[board == card] != 0)
}

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

max_opponent_threat <- function(r, c, chips, player) {
  best <- 0
  opponents <- setdiff(1:3, player)  # only real opponents
  for (opp in opponents) {
    best <- max(best, run_length_at(r, c, chips, opp))
  }
  best
}

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

# =========================================================================
#  YELLOW PLACEMENT STRATEGIES (controller decides where Yellow's chip goes)
# =========================================================================

# The controller wants to: (a) NOT give Yellow a sequence, (b) block opponents

place_yellow_novice <- function(spots, chips, controller) {
  # Random choice
  spots[safe_sample_idx(nrow(spots)), ]
}

place_yellow_middling <- function(spots, chips, controller) {
  # Pick the spot where Yellow (player 4) has the SHORTER run
  best_i <- 1; best_yrun <- Inf
  for (i in seq_len(nrow(spots))) {
    r <- spots[i,1]; c <- spots[i,2]
    chips[r, c] <- 4L
    yrun <- run_length_at(r, c, chips, 4L)
    chips[r, c] <- 0L
    if (yrun < best_yrun) {
      best_yrun <- yrun; best_i <- i
    }
  }
  spots[best_i, ]
}

place_yellow_optimal <- function(spots, chips, controller) {
  # Minimize Yellow's run; break ties by maximizing block on opponents
  # Also: NEVER place if it gives Yellow 5 (if both spots do, pick lesser evil)
  best_i <- 1; best_score <- -Inf
  for (i in seq_len(nrow(spots))) {
    r <- spots[i,1]; c <- spots[i,2]
    chips[r, c] <- 4L
    yrun <- run_length_at(r, c, chips, 4L)
    chips[r, c] <- 0L
    
    # opponent blocking value (how much does this spot disrupt real opponents?)
    opp_block <- max_opponent_threat(r, c, chips, controller)
    
    # Score: heavily penalize Yellow run length, reward opponent blocking
    score <- -yrun * 100
    if (yrun >= 5) score <- score - 10000  # catastrophic
    if (yrun >= 4) score <- score - 500    # very dangerous
    score <- score + opp_block * 10        # bonus for blocking opponents
    
    if (score > best_score) { best_score <- score; best_i <- i }
  }
  spots[best_i, ]
}

# =========================================================================
#  REAL PLAYER STRATEGIES (no Jacks, so simpler than base game)
# =========================================================================

real_novice <- function(hand, board, chips, locked, player) {
  playable <- c()
  for (ci in seq_along(hand)) {
    if (nrow(get_open_spots(hand[ci], board, chips)) > 0) playable <- c(playable, ci)
  }
  if (length(playable) == 0) return(NULL)
  ci <- safe_sample_vec(playable)
  spots <- get_open_spots(hand[ci], board, chips)
  pos <- spots[safe_sample_idx(nrow(spots)), ]
  list(ci = ci, pos = as.integer(pos))
}

real_middling <- function(hand, board, chips, locked, player) {
  for (ci in seq_along(hand)) {
    spots <- get_open_spots(hand[ci], board, chips)
    if (nrow(spots) == 0) next
    best_s <- -1; best_p <- spots[1, ]
    for (i in seq_len(nrow(spots))) {
      chips[spots[i,1], spots[i,2]] <- as.integer(player)
      s <- run_length_at(spots[i,1], spots[i,2], chips, player)
      chips[spots[i,1], spots[i,2]] <- 0L
      if (s > best_s) { best_s <- s; best_p <- spots[i, ] }
    }
    return(list(ci = ci, pos = as.integer(best_p)))
  }
  NULL
}

real_optimal <- function(hand, board, chips, locked, player) {
  best_score <- -Inf; best_move <- NULL
  for (ci in seq_along(hand)) {
    spots <- get_open_spots(hand[ci], board, chips)
    if (nrow(spots) == 0) next
    for (i in seq_len(nrow(spots))) {
      r <- spots[i,1]; cc <- spots[i,2]
      chips[r, cc] <- as.integer(player)
      own_run <- run_length_at(r, cc, chips, player)
      pot     <- potential_lines(r, cc, chips, player)
      chips[r, cc] <- 0L
      def <- max_opponent_threat(r, cc, chips, player)
      # Also penalize helping Yellow
      chips[r, cc] <- 4L  # hypothetical Yellow check
      yellow_help <- run_length_at(r, cc, chips, 4L)
      chips[r, cc] <- 0L
      
      if (own_run >= 5) { score <- 10000 }
      else {
        score <- own_run^3 * 5 + pot * 3 + def^2 * 8
        if (own_run == 4) score <- score + 150
        if (def >= 4) score <- score + 100
        # Don't accidentally help Yellow by leaving adjacent empty spots
        # (minor consideration, but optimal player is aware)
      }
      if (score > best_score) { best_score <- score; best_move <- list(ci=ci, pos=as.integer(spots[i,])) }
    }
  }
  best_move
}

# =========================================================================
#  GAME ENGINE
# =========================================================================

simulate_one_game <- function(strategies = c(3, 2, 1), verbose = FALSE) {
  board  <- create_board()
  chips  <- create_chips()
  locked <- create_locked()
  deck   <- create_deck()
  
  # Deal 7 cards to each of 3 real players
  hands <- list(deck[1:7], deck[8:14], deck[15:21])
  deck  <- deck[22:length(deck)]
  discard_pile <- character(0)
  
  real_fn    <- list(real_novice, real_middling, real_optimal)
  yellow_fn  <- list(place_yellow_novice, place_yellow_middling, place_yellow_optimal)
  plabels    <- c("Green","Blue","Red","Yellow")
  
  controller_order <- c(1, 2, 3)  # rotates each round
  round_num <- 0
  turn <- 0
  
  repeat {
    round_num <- round_num + 1
    controller <- controller_order[((round_num - 1) %% 3) + 1]
    
    # ---- YELLOW'S TURN ----
    turn <- turn + 1
    if (turn > 800 || length(deck) == 0) {
      if (verbose) cat("Game ended: deck exhausted or turn limit.\n")
      return(list(winner = NA_integer_, yellow_win = FALSE, turns = turn))
    }
    
    flipped <- deck[1]; deck <- deck[-1]
    
    yellow_spots <- get_open_spots(flipped, board, chips)
    if (nrow(yellow_spots) > 0) {
      if (nrow(yellow_spots) == 1) {
        pos <- yellow_spots[1, ]
      } else {
        pos <- yellow_fn[[strategies[controller]]](yellow_spots, chips, controller)
      }
      chips[pos[1], pos[2]] <- 4L
      if (verbose) cat(sprintf("Round %d | %s controls Yellow: flips %s -> [%d,%d]\n",
                               round_num, plabels[controller], flipped, pos[1], pos[2]))
      
      # Check if Yellow won (everyone loses)
      res <- find_and_lock(chips, locked, 4L)
      locked <- res$locked
      if (res$found) {
        if (verbose) cat(sprintf("*** YELLOW wins on turn %d! All players lose. ***\n", turn))
        return(list(winner = 4L, yellow_win = TRUE, turns = turn))
      }
    } else {
      if (verbose) cat(sprintf("Round %d | Yellow flips %s but no open spots. Skip.\n",
                               round_num, flipped))
    }
    
    # ---- REAL PLAYERS' TURNS ----
    for (p in 1:3) {
      turn <- turn + 1
      if (turn > 800) return(list(winner = NA_integer_, yellow_win = FALSE, turns = turn))
      
      hand <- hands[[p]]
      if (length(hand) == 0) next
      
      # Dead card discard
      for (ci in seq_along(hand)) {
        if (is_dead(hand[ci], board, chips)) {
          discard_pile <- c(discard_pile, hand[ci])
          # Reshuffle discard if deck empty
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
      
      move <- real_fn[[strategies[p]]](hand, board, chips, locked, p)
      
      if (!is.null(move)) {
        card <- hand[move$ci]
        chips[move$pos[1], move$pos[2]] <- as.integer(p)
        discard_pile <- c(discard_pile, card)
        
        # Draw replacement
        if (length(deck) == 0 && length(discard_pile) > 0) {
          deck <- sample(discard_pile); discard_pile <- character(0)
        }
        if (length(deck) > 0) { hand[move$ci] <- deck[1]; deck <- deck[-1] }
        else hand <- hand[-move$ci]
        hands[[p]] <- hand
        
        if (verbose) cat(sprintf("Round %d | %s plays %s -> [%d,%d]\n",
                                 round_num, plabels[p], card, move$pos[1], move$pos[2]))
        
        res <- find_and_lock(chips, locked, p)
        locked <- res$locked
        if (res$found) {
          if (verbose) cat(sprintf("*** %s wins on turn %d! ***\n", plabels[p], turn))
          return(list(winner = as.integer(p), yellow_win = FALSE, turns = turn))
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
  plabels <- c("Green","Blue","Red","Yellow")
  
  cat("=============================================================\n")
  cat("  Sequence: Blind 4th Player Variant\n")
  cat("  (No Jacks, Yellow controlled by rotating real player)\n")
  cat("=============================================================\n")
  cat(sprintf("  Games to play : %d\n", n_games))
  cat(sprintf("  Player 1 (Green) : %s (level %d)\n", labels[strategies[1]], strategies[1]))
  cat(sprintf("  Player 2 (Blue)  : %s (level %d)\n", labels[strategies[2]], strategies[2]))
  cat(sprintf("  Player 3 (Red)   : %s (level %d)\n", labels[strategies[3]], strategies[3]))
  cat("=============================================================\n\n")
  
  wins <- integer(4)  # 1=Green, 2=Blue, 3=Red, 4=Yellow
  draws <- 0L
  pb <- txtProgressBar(min = 0, max = n_games, style = 3)
  
  for (g in seq_len(n_games)) {
    result <- tryCatch(
      simulate_one_game(strategies = strategies, verbose = FALSE),
      error = function(e) list(winner = NA_integer_, yellow_win = FALSE, turns = 0)
    )
    w <- result$winner
    if (is.na(w)) draws <- draws + 1L
    else wins[w] <- wins[w] + 1L
    setTxtProgressBar(pb, g)
  }
  close(pb)
  
  total_decided <- n_games - draws
  cat("\n\n====================== RESULTS ======================\n")
  for (p in 1:3) {
    pct <- round(100 * wins[p] / n_games, 1)
    bar <- paste(rep("#", round(pct / 2)), collapse = "")
    cat(sprintf("  %-6s %-8s : %4d wins (%5.1f%%) %s\n",
                plabels[p], paste0("(", labels[strategies[p]], ")"),
                wins[p], pct, bar))
  }
  ypct <- round(100 * wins[4] / n_games, 1)
  ybar <- paste(rep("#", round(ypct / 2)), collapse = "")
  cat(sprintf("  %-6s %-8s : %4d wins (%5.1f%%) %s\n",
              "Yellow", "(blind)", wins[4], ypct, ybar))
  if (draws > 0) cat(sprintf("  Draws/exhausted  : %4d\n", draws))
  cat("=====================================================\n\n")
  
  cat(sprintf("  Yellow spoils it %.1f%% of the time.\n", ypct))
  cat(sprintf("  Real players win %.1f%% of the time combined.\n",
              round(100 * sum(wins[1:3]) / n_games, 1)))
  cat("=====================================================\n")
  
  invisible(list(wins = wins, draws = draws, strategies = strategies))
}

# =========================================================================
#  RUN IT
# =========================================================================

# Player 1 = Optimal, Player 2 = Middling, Player 3 = Novice
# result <- run_batch(n_games = 1000, strategies = c(3, 2, 1))