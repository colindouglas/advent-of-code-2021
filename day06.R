library(tidyverse)

validation <- read_lines('day06-validation.txt') %>%
  str_split(pattern = ",", simplify = TRUE) %>%
  as.numeric()

input <- read_lines('day06-input.txt') %>%
  str_split(pattern = ",", simplify = TRUE) %>%
  as.numeric()

new_fish_timer <- 8
old_fish_timer <- 6

tick_timer <- function(x) {
  x_new <- x - 1  # Decrease timer
  new_fish <- sum(x_new == -1)  # How many new fish?
  x_new <- c(x_new, rep(new_fish_timer, times = new_fish))  # Add new fish
  x_new[x_new == -1] <- old_fish_timer # Reset old fish
  x_new
}


validation_1 <- c(2,3,2,0,1)
validation_2 <- c(1,2,1,6,0,8)
validation_18 <- c(6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8)

tick_timer(validation) == validation_1
many_ticks <- function(init, n) {
  for (nothing in seq_len(n)) {
    init <- tick_timer(init)
  }
  init
}

all(many_ticks(validation, 1) == validation_1)
all(many_ticks(validation, 2) == validation_2)
all(many_ticks(validation, 18) == validation_18)


part1 <- length(many_ticks(input, 80))



# Part 2 ------------------------------------------------------------------

# Fish tick counter

vec_to_counter <- function(vec) {
  
  counter <- vector(mode = "numeric", length = 10)
  names(counter) <- as.character((-1):8)
  tbl <- table(vec)
  
  for (n in (-1):8) {
    n_chr <- as.character(n)
    out[n_chr] <- counter[n_chr] + tbl[n_chr]
  }
  
  out[is.na(out)] <- 0
  
  out
}

step_timer2 <- function(counter) {
  
  for (n in 0:8) {
    from <- as.character(n)
    to <- as.character(n - 1)
    out[to] <- counter[from]
  }
  
  out[as.character(new_fish_timer)] <- out["-1"]
  out[as.character(old_fish_timer)] <- out[as.character(old_fish_timer)] + out["-1"]
  out["-1"] <- 0
  out
}


many_ticks2 <- function(init, n) {
  for (nothing in seq_len(n)) {
    init <- step_timer2(init)
  }
  init
}

options(digits=22)

vec_to_counter(input) %>%
  many_ticks2(., n = 256) %>%
  sum()
