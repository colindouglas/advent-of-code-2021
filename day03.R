library(tidyverse)

input <- read_lines("day03-input.txt") %>%
  tibble(row = .) %>%
  separate(row, into = paste0("pos_", sprintf("%02d", 1:12)), sep = 1:12, remove = FALSE)


most_common <- function(x) {
  
  tbx <- table(x)
  if (tbx[1] == tbx[2]) return("1")
  names(tbx)[tbx == max(tbx)]
  
}

least_common <- function(x) {
  
  tbx <- table(x)
  if (tbx[1] == tbx[2]) return("0")
  names(tbx)[tbx == min(tbx)]
  
}


strtoi(paste0(map_chr(input, most_common), collapse = ""), base = 2) *
strtoi(paste0(map_chr(input, least_common), collapse = ""), base = 2)


# Part 2
this_func <- most_common
oxygen <- input %>%
  filter(pos_01 == this_func(pos_01)) %>%
  filter(pos_02 == this_func(pos_02)) %>%
  filter(pos_03 == this_func(pos_03)) %>%
  filter(pos_04 == this_func(pos_04)) %>%
  filter(pos_05 == this_func(pos_05)) %>%
  filter(pos_06 == this_func(pos_06)) %>%
  filter(pos_07 == this_func(pos_07)) %>%
  filter(pos_08 == this_func(pos_08)) %>%
  filter(pos_09 == this_func(pos_09)) %>%
  filter(pos_10 == this_func(pos_10)) %>%
  filter(pos_11 == this_func(pos_11)) %>%
  filter(pos_12 == this_func(pos_12)) %>%
  pull(row) %>%
  strtoi(base = 2)

this_func <- least_common
co2 <- input %>%
  filter(pos_01 == this_func(pos_01)) %>%
  filter(pos_02 == this_func(pos_02)) %>%
  filter(pos_03 == this_func(pos_03)) %>%
  filter(pos_04 == this_func(pos_04)) %>%
  filter(pos_05 == this_func(pos_05)) %>%
  filter(pos_06 == this_func(pos_06)) %>%
  filter(pos_07 == this_func(pos_07)) %>%
  filter(pos_08 == this_func(pos_08)) %>%
  filter(pos_09 == this_func(pos_09)) %>%
  pull(row) %>%
  strtoi(base = 2)

oxygen * co2
