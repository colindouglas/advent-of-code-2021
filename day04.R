library(tidyverse)

input <- read_lines("day04-input.txt")

called_numbers <- as.numeric(as.vector(str_split(input[1], ",", simplify = TRUE)))



parse_card <- function(input, start) {
  
  out <- input[start:(start + 4)] %>%
    str_trim(side = "both") %>%  # Trim leading and lagging white space
    str_split("\\s+", simplify = TRUE)  # Match any number of spaces
  
  class(out) <- "numeric"
  stopifnot(dim(out) == c(5, 5))
  out
}

starting_lines <- seq(from = 3, to = length(input), by = 6)

cards <- map(starting_lines, ~ parse_card(input, .))

# Mark a single number on a single card
mark_number <- function(card, called) {
  stopifnot(length(called) == 1)
  card[card == called] <- -1
  card
}

# Mark a single number on every card
mark_cards <- function(cards, called) {
  
  stopifnot(length(called) == 1)
  map(cards, ~ mark_number(., called))
  
  
}

# Check if a card is a winner
check_winner <- function(card) {
  any(rowSums(card) == -5) | any(colSums(card) == -5)
}


# Check all the cards and see if one wins

check_winners <- function(cards) {
  winners <- map_lgl(cards, check_winner)
  if (any(winners)) {
    return(which(winners))
  } else {
    return(0)
  }
}


# Play the game
marked_cards <- cards
for (n in called_numbers) {
  
  marked_cards <- mark_cards(marked_cards, n)
  winner <- check_winners(marked_cards)
  
  if (winner > 0) {
    print(winner)
    last_called <- n
    winning_card <- marked_cards[[winner]]
    break
  }
}

sum(winning_card[winning_card >= 0]) * n



# Part 2 ------------------------------------------------------------------

remove_winners <- function(cards) {
  winners <- map_lgl(cards, check_winner)
  cards[!winners]
}

marked_cards <- cards

for (n in called_numbers) {
  
  marked_cards <- mark_cards(marked_cards, n)

  if (length(marked_cards) > 1) {
    marked_cards <- remove_winners(marked_cards)
    has_finally_won <- FALSE
  } else {
    has_finally_won <- check_winners(marked_cards) > 0
  }
  
  if (has_finally_won) {
    last_called <- n
    last_card <- first(marked_cards)
    break
  }
  
}


sum(last_card[last_card >= 0]) * last_called
