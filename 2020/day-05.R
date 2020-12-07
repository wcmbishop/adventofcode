library(dplyr)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
input_raw <- file.path("data-raw", "day-05", "boarding-passes.txt") %>% 
  readLines()


# part 1 ====
binary_parser <- function(str, start, end, low_char) {
  vals <- seq(start, end)
  # take upper or lower half of values
  mid <- floor(length(vals) / 2)
  next_vals <- vals[c(1:mid)*ifelse(str_sub(str, 1, 1) == low_char, 1, -1)]
  if (length(next_vals) == 1) {
    return(next_vals)
  } else {
    # recurse to next character
    binary_parser(
      str = str_sub(str, 2, -1), 
      start = min(next_vals), 
      end = max(next_vals),
      low_char = low_char
    )
  }
}
parse_row <- function(str, start = 0, end = 127) {
  binary_parser(str, start = start, end = end, low_char = "F")
}
parse_col <- function(str, start = 0, end = 7) {
  binary_parser(str, start = start, end = end, low_char = "L")
}
parse_boarding_pass <- function(x) {
  row_str <- str_sub(x, 1, 7)
  col_str <- str_sub(x, 8, 10)
  row <- parse_row(row_str)
  col <- parse_col(col_str)
  c("row" = row, "col" = col)
}
calculate_seat_id <- function(row, col) {
  setNames(row * 8 + col, "id")
}

# SAMPLES:
# BFFFBBFRRR: row 70, column 7, seat ID 567.
seat <- parse_boarding_pass("BFFFBBFRRR")
expect_equivalent(seat, c(70, 7))
expect_equivalent(calculate_seat_id(seat['row'], seat['col']), 567)
# FFFBBBFRRR: row 14, column 7, seat ID 119.
seat <- parse_boarding_pass("FFFBBBFRRR")
expect_equivalent(seat, c(14, 7))
expect_equivalent(calculate_seat_id(seat['row'], seat['col']), 119)
# BBFFBBFRLL: row 102, column 4, seat ID 820.
seat <- parse_boarding_pass("BBFFBBFRLL")
expect_equivalent(seat, c(102, 4))
expect_equivalent(calculate_seat_id(seat['row'], seat['col']), 820.)

# process all boarding passes
passes <- tibble(boarding_pass = input_raw) %>% 
  mutate(
    seat = map(boarding_pass, ~parse_boarding_pass(.)),
    seat_row = map_dbl(seat, ~.["row"]),
    seat_col = map_dbl(seat, ~.["col"]),
    seat_id = map_dbl(seat, ~calculate_seat_id(.["row"], .["col"]))
  )
max(passes$seat_id)

# part 2 ====
# look for a jump in the seat ids
passes %>% 
  arrange(seat_id) %>% 
  mutate(id_delta = seat_id - lag(seat_id)) %>% 
  filter(id_delta > 1)
