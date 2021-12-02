library(tidyverse)

input <- read_csv("day02-input.txt", col_names = FALSE)[[1]]

forward <- input[str_detect(input, "forward")]  %>%
  str_replace("forward ", "") %>%
  as.numeric()

updown <- input[!str_detect(input, "forward")] %>%
  str_replace("up ", "-") %>%
  str_replace("down ", "") %>%
  as.numeric()

sum(forward) * sum(updown)  


# Part 2


# down X increases your aim by X units.
# up X decreases your aim by X units.
# forward X does two things:
#   It increases your horizontal position by X units.
#   It increases your depth by your aim multiplied by X.



df_input <- tibble(command = input) %>%
  separate(command, into = c("dir", "mag")) %>%
  mutate(
    mag = as.numeric(mag),
    delta_aim = case_when(
      dir == "down" ~ mag, 
      dir == "up" ~ -mag,
      TRUE ~ 0),
    aim = cumsum(delta_aim))

df_pos <- df_input %>%
  filter(dir == "forward") %>%
  mutate(pos_hor = cumsum(mag),
         pos_ver = cumsum(mag * aim))

last(df_pos$pos_hor) * last(df_pos$pos_ver)
