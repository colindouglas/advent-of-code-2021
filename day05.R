library(tidyverse)

input <- read_lines("day05-input.txt")

lines <- as.tibble(str_split(input, pattern = ",|( -> )", simplify = TRUE))
names(lines) <- c("x1", "y1", "x2", "y2")

lines <- lines %>%
  mutate(across(everything(), as.numeric))

# For now, only consider horizontal and vertical lines
lines_1 <- lines %>%
  filter(x1 == x2 | y1 == y2)



# Make a matrix of zeros
field <- matrix(0L, 
                nrow = max(lines$x1, lines$y1, lines$x2, lines$y2)+1,
                ncol = max(lines$x1, lines$y1, lines$x2, lines$y2)+1)



add_lines <- function(x1, y1, x2, y2) {
  
  if (x1 == x2) {
    movement <- seq(from = y1, to = y2, by = -sign(y1 - y2))
    for (n in movement) {
      field[n+1, x1+1] <- field[n+1, x1+1] + 1
      #message(paste(n, x1))
    }
  } else if (y1 == y2) {
    movement <- seq(from = x1, to = x2, by = -sign(x1 - x2))
    for (n in movement) {
      field[y1+1, n+1] <- field[y1+1, n+1] + 1
      #message(paste(y1, n))
    }
  }
  field
}


for (i in seq_len(nrow(lines_1))) {
  row <- lines_1[i,]
  field <- add_lines(row$x1, row$y1, row$x2, row$y2)
}

sum(field > 1)


# Part 2 ------------------------------------------------------------------

# Make a matrix of zeros
field <- matrix(0L, 
                nrow = max(lines$x1, lines$y1, lines$x2, lines$y2)+1,
                ncol = max(lines$x1, lines$y1, lines$x2, lines$y2)+1)

add_lines_diag <- function(x1, y1, x2, y2) {
  
  if (x1 == x2) {
    movement <- seq(from = y1, to = y2, by = -sign(y1 - y2))
    for (n in movement) {
      field[n+1, x1+1] <- field[n+1, x1+1] + 1
      #message(paste(n, x1))
    }
  } else if (y1 == y2) {
    movement <- seq(from = x1, to = x2, by = -sign(x1 - x2))
    for (n in movement) {
      field[y1+1, n+1] <- field[y1+1, n+1] + 1
      #message(paste(y1, n))
    }
  } else {
    movement_x <- seq(from = x1, to = x2, by = -sign(x1 - x2))
    movement_y <- seq(from = y1, to = y2, by = -sign(y1 - y2))
    
    stopifnot(length(movement_x) == length(movement_y))  # Needs to be diagonal
    
    for (i in seq_along(movement_x)) {
      #message(i)
      #message(paste(movement_y[i]+1, movement_x[i]+1))
      field[movement_y[i]+1, movement_x[i]+1] <- field[movement_y[i]+1, movement_x[i]+1] + 1
    }
  }
  field
}

for (i in seq_len(nrow(lines))) {
  
  row <- lines[i,]
  field <- add_lines_diag(row$x1, row$y1, row$x2, row$y2)
}

sum(field > 1)
