library(tidyverse)

input_raw <- read_lines("day11-input.txt")

d <- length(input_raw)

input <- input_raw %>%
  str_split(pattern = "", simplify = TRUE) %>%
  as.numeric() %>%
  matrix(nrow = d)


flash <- function(m, i, j) {

  flash_counter <<- flash_counter + 1  # global variable idc

  try({m[i-1, j-1] <- m[i-1, j-1] + 1}, silent = TRUE)
  try({m[i-1, j  ] <- m[i-1, j  ] + 1}, silent = TRUE)
  try({m[i-1, j+1] <- m[i-1, j+1] + 1}, silent = TRUE)

  try({m[i+1, j-1] <- m[i+1, j-1] + 1}, silent = TRUE)
  try({m[i+1, j  ] <- m[i+1, j  ] + 1}, silent = TRUE)
  try({m[i+1, j+1] <- m[i+1, j+1] + 1}, silent = TRUE)

  try({m[i  , j-1] <- m[i  , j-1] + 1}, silent = TRUE)
  try({m[i  , j+1] <- m[i  , j+1] + 1}, silent = TRUE)

  m[i,j] <- NA
  m
}

find_flashers <- function(m) {

  f <- which(m > 9)
  i <- (f-1) %% d + 1
  j <- (f-1) %/% d + 1
  out <- map2(i, j, ~ c(..1, ..2))

}

step <- function(m) {

  # First, the energy level of each octopus increases by 1.
  m <- m + 1

  flashers <- find_flashers(m)
  loop_counter <- 0

  while (length(flashers) > 0) {

    for (i in seq_along(flashers)) {
      m <- flash(m, i = flashers[[i]][1], j = flashers[[i]][2])
    }
    flashers <- find_flashers(m)
  }

  m[is.na(m)] <- 0
  m
}

flash_counter <- 0
m <- input

for (i in 1:100) {
  m <- step(m)
}

message(flash_counter)


# Part 2 ------------------------------------------------------------------

for (n_step in 1:2000) {
  m <- step(m)
  if (all(m == 0)) break
}

message(n_step)
