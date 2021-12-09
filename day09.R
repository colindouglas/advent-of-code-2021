library(tidyverse)

input_raw <- read_lines("day09-input.txt") 

input <- input_raw %>% 
  str_split(pattern = "") %>%
  unlist() %>%
  as.numeric() %>%
  matrix(nrow = length(input_raw), byrow = TRUE)

input_rows <- dim(input)[1]
input_cols <- dim(input)[2]

is_lowpoint <- function(matrix, i, j) {
  
  d <- dim(matrix)  # Rows, columns
  
  up <- ifelse(i > 1, matrix[i-1, j], NA)
  down <- ifelse(i < d[1], matrix[i+1, j], NA)
  left <- ifelse(j > 1, matrix[i, j-1], NA)
  right <- ifelse(j < d[2], matrix[i, j+1], NA)
  
  adjacent_cells <- c(up, down, left, right)
  adjacent_cells <- adjacent_cells[!is.na(adjacent_cells)]
  
  all(matrix[i, j] < adjacent_cells)
}

out <- matrix(0L, nrow = input_rows, ncol = input_cols)

for (i in seq_len(input_rows)) {
  for (j in seq_len(input_cols)) {
    if (is_lowpoint(input, i, j)) {
      out[i,j] <- input[i,j] + 1
    }
  }
}

sum(out)



# Part 2 ------------------------------------------------------------------

smallest_neighbor <- function(matrix, i, j) {
  
  d <- dim(matrix)  # Rows, columns
  
  up <- ifelse(i > 1, matrix[i-1, j], NA)
  down <- ifelse(i < d[1], matrix[i+1, j], NA)
  left <- ifelse(j > 1, matrix[i, j-1], NA)
  right <- ifelse(j < d[2], matrix[i, j+1], NA)
  
  adjacent_cells <- c(up, down, left, right)
  smallest_adjacent <- min(adjacent_cells, na.rm = TRUE)
  at_min <- matrix[i, j] <= smallest_adjacent
  
  case_when(
    at_min ~ c(i, j),
    isTRUE(up == smallest_adjacent) ~ c(i-1L, j),
    isTRUE(down == smallest_adjacent) ~ c(i+1L, j),
    isTRUE(left == smallest_adjacent) ~ c(i, j-1L),
    isTRUE(right == smallest_adjacent) ~ c(i, j+1L)
  )
  
}

# For each cell, trace it back to where it terminates

# These ints identify each matrix position
cell_numbers <- matrix(seq_len(length(input)), nrow = input_rows, ncol = input_cols)

# Could probably speed this up by looking to see if we've gotten to a position
# that's already been solved.
trace_to_bottom <- function(matrix, i, j) {
  next_position <- c(i, j)
  # while (TRUE) {
  for (nothing in 1:1000) {
    last_position <- next_position
    next_position <- smallest_neighbor(matrix, last_position[1], last_position[2])
    if (all(next_position == last_position)) {
      return(cell_numbers[next_position[1], next_position[2]])
    }
  }
}

bottoms <- matrix(0, nrow = input_rows, ncol = input_cols)

for (i in seq_len(input_rows)) {
  for (j in seq_len(input_cols)) {
    if(input[i,j] == 9) {
      bottoms[i,j] <- 0  # 9s don't count as part of a basin
    } else {
      bottoms[i,j] <- trace_to_bottom(input, i, j)
    }
  }
}

tbl <- table(bottoms)
tbl <- tbl[names(tbl) != 0]  # Ignore 9s

sort(tbl, decreasing = TRUE)[1:3] %>% reduce(`*`)
