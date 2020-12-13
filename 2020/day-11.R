library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
input_raw <- file.path("data-raw", "day-11", "seats.txt") %>% 
  readLines()
example <- file.path("data-raw", "day-11", "example.txt") %>% 
  readLines()
EMPTY <- "L"
FLOOR <- "."
OCCUPIED <- "#"

# part 1 ====
# build seat map matrix
seat_map <- matrix(
  str_split(input_raw, "") %>% unlist(), 
  nrow = length(input_raw), 
  ncol = nchar(input_raw[1])
)
example_map <- matrix(
  str_split(example, "") %>% unlist(), 
  nrow = length(example), 
  ncol = nchar(example[1])
)

# adjacent_seats <- function(seats, row, col) {
#   rows <- row + c(-1:1)
#   cols <- col + c(-1:1)
#   seats[row, col] <- "C"
#   dim <- dim(seats)
#   adjacent <- seats[
#     rows[rows >= 1 & rows <= dim[1]],
#     cols[cols >= 1 & cols <= dim[2]]
#   ]
#   adjacent[adjacent != "C"]
# }

occupied_neighbors <- function(seats, row, col) {
  rows <- row + c(-1:1)
  cols <- col + c(-1:1)
  dim <- dim(seats)
  adjacent <- seats[
    rows[rows >= 1 & rows <= dim[1]],
    cols[cols >= 1 & cols <= dim[2]]
    ]
  length(adjacent[adjacent == OCCUPIED]) - (seats[row, col] == OCCUPIED)
}

update_seats <- function(seats) {
  new_seats <- seats
  for (row in seq_len(nrow(seats))) {
    for (col in which(seats[row, ] != FLOOR)) {
      seat <- seats[row, col]
      neighbors <- occupied_neighbors(seats, row, col)
      if (seat == EMPTY && neighbors == 0) {
        new_seats[row, col] <- OCCUPIED
      } else if (seat == OCCUPIED && neighbors >= 4) {
        new_seats[row, col] <- EMPTY
      }
    }
  }
  new_seats
}

find_stable_seating <- function(seats) {
  new_seats <- update_seats(seats)
  round <- 1
  repeat {
    if (identical(seats, new_seats)) {
      print("seating is stable!")
      break
    }
    if (round %% 100 == 0) {
      print(glue("{time}: seating shuffle, round {round}...",
                 time = Sys.time(), round = round))
    }
    seats <- new_seats
    new_seats <- update_seats(seats)
    round <- round + 1
  }
  seats
}

find_stable_seating(example_map)
find_stable_seating(seat_map)

profvis::profvis(
  update_seats(seat_map)
)


# part 2 ====
