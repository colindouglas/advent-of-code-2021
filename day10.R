library(tidyverse)

input <- read_lines("day10-input.txt")

# Removing adjacent pairs of parens in a string
remove_pairs <- function(x) {
  str_remove_all(x, pattern = "(\\(\\))|(\\{\\})|(\\[\\])|(\\<\\>)")
}

# Iteratively remove adjacent pairs of parens in a string
# until there's none left to remove
remove_pairs_iter <- function(x) {
  #while(TRUE) {
  for (i in 1:100) {
    old <- x
    x <- remove_pairs(x)
    if (x == old) return(x)
  }
}

# A line is just "incomplete" if, after removal, it only has left parens
is_incomplete <- function(x) {
  x <- str_remove_all(x, pattern = "\\(|\\{|\\<|\\[")
  x == ""
}

# A line is corrupt if it has a mix of parens after iterative removal
is_corrupt <- function(x) {
  !is_incomplete(x)
}

# Return the first right paren in a string
# Returns NA if the string is not corrupt
get_first_right_paren <- function(x) {
  str_extract(x, pattern = "\\)|\\}|\\>|\\]")
}

illegal_char_values <- c(
  ")" = 3,
  "]" = 57,
  "}" = 1197,
  ">" = 25137
)

input

input_stripped <- map_chr(input, remove_pairs_iter)   # With pairs removed
which_corrupt <- map_lgl(input_stripped, is_corrupt)  # Which lines are corrupt
input_corrupt <- input_stripped[which_corrupt]        # Corrupt lines
illegal_chars <- map_chr(input_corrupt, get_first_right_paren)  # First illegal char
points <- sum(illegal_char_values[illegal_chars])     # Score of lines
message(points)


# Part 2 ------------------------------------------------------------------

input_incomplete <- input_stripped[!which_corrupt]

get_completion_string <- function(x) {
  out <- as.vector(str_split(x, pattern = "", simplify = TRUE))
  out <- paste(rev(out), collapse = "")

  # We don't need to do this, we could just look up it's pair
  # But let's do it for clarity sake
  out %>%
    str_replace_all(pattern = "\\(", "\\)") %>%
    str_replace_all(pattern = "\\[", "\\]") %>%
    str_replace_all(pattern = "\\{", "\\}") %>%
    str_replace_all(pattern = "\\<", "\\>")

}

completion_strings <- map_chr(input_incomplete, get_completion_string)

char_vals <- c(
  ")" = 1,
  "]" = 2,
  "}" = 3,
  ">" = 4)

# Score a completion string according to the rules defined in the question
score_completion_string <- function(str) {
  str_ <- first(str_split(str, pattern = ""))

  counter <- 0

  for (char in str_) {
    counter <- counter * 5
    counter <- counter + char_vals[char]
  }

  counter
}

# Scores of each incomplete line
scores <- map_dbl(completion_strings, score_completion_string)
message(median(scores))  # Median gives us the one in the middle

