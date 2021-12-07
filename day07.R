library(tidyverse)

input <- read_lines("day07-input.txt") %>%
  str_split(pattern = ",", simplify = TRUE) %>%
  as.numeric()

sum(abs(input - median(input)))


# Part 2 ------------------------------------------------------------------


# The cost of one crab moving to a given position
cost_one_crab <- function(start, end) {
  sum(seq_len(abs(start - end)))
}

# The cost of all crabs moving to a given position
cost_all_crabs <- function(positions, meet) {
  sum(map_dbl(positions, ~ cost_one_crab(., meet)))
}

# The answer is bounded by the min/max current position
lowest_pos <- min(input)
highest_pos <- max(input)

# The cost of moving to every position
out <- map_dbl(lowest_pos:highest_pos, ~ cost_all_crabs(input, .))

# The smallest cost
min(out)

