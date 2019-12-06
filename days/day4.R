# DAY 4
library(dplyr)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)


# part 1 ====
# constraints:
# - It is a six-digit number.
# - The value is within the range given in your puzzle input.
# - Two adjacent digits are the same (like 22 in 122345).
# - Going from left to right, the digits never decrease;
#   they only ever increase or stay the same (like 111123 or 135679).
range <- c(273025, 767253)
passwords <- tibble(value = seq(range[1], range[2])) %>% 
  mutate(digits = map(value, ~str_split(.x, "")[[1]]),
         nchar = nchar(value),
         run_lengths = map(digits, ~rle(.x)$lengths),
         max_rle = map_int(run_lengths, max),
         min_diff = map_int(digits, ~min(diff(as.integer(.x))))) %>% 
  filter(max_rle > 1, min_diff >= 0, nchar == 6,
         between(value, range[1], range[2]))
passwords

# part 2 ====
# - the two adjacent matching digits are not part of a larger group of matching digits.
passwords %>% 
  mutate(rle_2 = map_lgl(run_lengths, ~any(.x == 2))) %>% 
  filter(rle_2 == TRUE)

