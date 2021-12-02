library(tidyverse)

input <- read_csv("day01-input.txt", col_names = FALSE)[[1]]

sum(lead(input, 1) > input, na.rm = TRUE)

# Part 2

sum(zoo::rollsum(lead(input, 1), 3) > zoo::rollsum(input, 3), na.rm = TRUE)
