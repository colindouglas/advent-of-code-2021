library(tidyverse)

input <- read_lines("day08-input.txt") %>%
  str_split(pattern = " \\| ", simplify = TRUE)

patterns <- input[,1] %>% 
  str_split(pattern = " ", simplify = TRUE)

outputs <- str_trim(input[,2])


# 1 is 2 segments
# 4 is 4 segments
# 7 is 3 segments
# 8 is 7 segments


outputs %>% 
  str_split(pattern = " ") %>% 
  unlist() %>% 
  map_dbl(., nchar) %>%
  {. %in% c(2, 4, 3, 7)} %>% 
  sum()


# Part 2 ------------------------------------------------------------------

single_check_contains <- function(x, contains) {
  x <- str_split(x, pattern = "", simplify = TRUE)[1,]
  contains <- str_split(contains, pattern = "", simplify = TRUE)[1,]
  all(contains %in% x)
}

check_contains <- function(x, contains) {
  map_lgl(x, ~ single_check_contains(., contains))
}

# Find if a string contains a - g
letter_freq <- function(x) {
  out <- vector("logical", length = 8)
  for (i in 1:8) {
    out[i] <- as.numeric(grepl(letters[i], x))
  }
  paste0(out, collapse = "")
}

# Sort a word alphabetically
sort_word <- function(word) {
  stopifnot(!grepl(" ", word))
  paste(sort(str_split(word, pattern = "")[[1]]), collapse = "")
}

sort_words <- function(words) {
  words_ <- str_split(words, pattern = " ")[[1]]
  words_ <- map_chr(words_, sort_word)
  words_ <- paste(words_, collapse = " ")
  words_
}


# Read in data ------------------------------------------------------------

signal_patterns <- map_chr(input[,1], sort_words)
outputs <- map_chr(str_trim(input[,2]), sort_words)

decode <- function(pattern) {
  
  pattern <- str_split(pattern, pattern = " ", simplify = TRUE)[1,]
  
  # 1, 4, 7, and 8 are easy
  
  one <- pattern[nchar(pattern) == 2]
  four <- pattern[nchar(pattern) == 4]
  seven <- pattern[nchar(pattern) == 3]
  eight <- pattern[nchar(pattern) == 7]
  
  assigned <- c(one, four, seven, eight)
  unassigned <- pattern[!(pattern %in% assigned)]
  
  # Of what's left, 9 contains all of four
  nine <- unassigned[check_contains(unassigned, four)]
  
  assigned <- append(assigned, nine)
  unassigned <- setdiff(pattern, assigned)
  
  # Of what's left, zero contains all of 1 and is 6 chars long
  zero <- unassigned[check_contains(unassigned, one) & nchar(unassigned) == 6]
  
  assigned <- append(assigned, zero)
  unassigned <- setdiff(pattern, assigned)
  
  # The last 6 char number must be 6
  
  six <- unassigned[nchar(unassigned) == 6]
  assigned <- append(assigned, six)
  unassigned <- setdiff(pattern, assigned)
  
  
  # That leaves 2, 3, and 5
  # 3 is the only one that contains all the same squares as 1
  three <- unassigned[check_contains(unassigned, one)]
  
  assigned <- append(assigned, three)
  unassigned <- setdiff(pattern, assigned)
  
  # 6 contains all of 5
  five <- unassigned[map_lgl(unassigned, ~ check_contains(six, .))]
  
  assigned <- append(assigned, five)
  unassigned <- setdiff(pattern, assigned)
  
  
  # The last one left is 2
  stopifnot(length(unassigned) == 1)
  two <- unassigned
  
  c(one, two, three, four, five, six, seven, eight, nine, zero)
  
}


# Ah but they're scrambled

translate <- function(signal_pattern, output_value) {
  
  decoder <- decode(signal_pattern)
  output_value <- str_split(output_value, pattern = " ", simplify = TRUE)[1,]
  out <- map_dbl(output_value, ~ which(. == decoder) %% 10)
  as.numeric(paste(out, collapse = ""))
  
}


output_values_ints <- map_dbl(seq_along(signal_patterns), ~ translate(signal_patterns[.], outputs[.]))
sum(output_values_ints)
