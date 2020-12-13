library(dplyr)
library(glue)
library(OpenImageR)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
input_raw <- file.path("data-raw", "day-11", "seats.txt") %>% 
  readLines()
example_raw <- file.path("data-raw", "day-11", "example.txt") %>% 
  readLines()
EMPTY <- "L"
FLOOR <- "."
OCCUPIED <- "#"

# part 1 ====
# build seat map matrix
build_seat_map <- function(text_lines) {
  matrix(
    text_lines %>% str_split("") %>% unlist(), 
    nrow = length(text_lines), 
    ncol = nchar(text_lines[1]),
    byrow = TRUE
  )
}

occupied_neighbors <- function(seats, row, col) {
  rows <- row + c(-1:1)
  cols <- col + c(-1:1)
  dim <- dim(seats)
  adjacent <- seats[
    rows[rows >= 1 & rows <= dim[1]],
    cols[cols >= 1 & cols <= dim[2]]
    ]
  sum(adjacent == OCCUPIED) - (seats[row, col] == OCCUPIED)
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

update_seats_fast <- function(seats) {
  mask <- matrix(1, ncol = 3, nrow = 3)
  neighbors <- convolution(
    matrix(1, ncol = ncol(seats), nrow = nrow(seats)),
    kernel = mask,
    mode = "same"
  )
  is_empty <- seats == EMPTY
  is_occupied <- seats == OCCUPIED
  empty <- !is_occupied
  class(empty) <- "numeric"
  occupied <- is_occupied
  class(occupied) <- "numeric"
  # turn emptyto to occupied
  seats[convolution(empty, mask, "same") == neighbors & is_empty] <- OCCUPIED
  # turn occupied to empty
  seats[convolution(occupied, mask, "same") > 4 & is_occupied] <- EMPTY
  seats
}

find_stable_seating <- function(seats, update_fun = update_seats, ...) {
  new_seats <- update_fun(seats, ...)
  repeat {
    if (identical(seats, new_seats)) break
    seats <- new_seats
    new_seats <- update_fun(seats, ...)
  }
  seats
}

# * test example ====
example_map <- build_seat_map(example_raw)
stable_seats <- find_stable_seating(example_map)
expect_equal(sum(stable_seats == OCCUPIED), 37)

# * sort real seat map ====
seat_map <- build_seat_map(input_raw)
stable_seats <- find_stable_seating(seat_map)
sum(stable_seats == OCCUPIED)
# 2386

# * measure speed difference ====
microbenchmark::microbenchmark(
  find_stable_seating(seat_map, update_fun = update_seats),
  find_stable_seating(seat_map, update_fun = update_seats_fast),
  times = 10L
)
# with update_seats: median 3310ms
# with update_seats_fast: medin 87ms


# part 2 ====

visible_neighbor <- function(seats, row, col, direction) {
  dim <- dim(seats)
  # find all seats in line-of-site
  # - directions: N, NE, E, SE, S, SW, W, NW
  if (direction == "N") {
    los <- seats[row:1 %>% .[. != row], col]
  } else if (direction == "NE") {
    rows <- row:1 %>% .[. != row]
    cols <- col:dim[2] %>% .[. != col]
    len <- min(length(rows), length(cols))
    los <- map2_chr(rows[seq_len(len)], cols[seq_len(len)], ~seats[.x, .y])
  } else if (direction == "E") {
    los <- seats[row, col:dim[2] %>% .[. != col]]
  } else if (direction == "SE") {
    rows <- row:dim[1] %>% .[. != row]
    cols <- col:dim[2] %>% .[. != col]
    len <- min(length(rows), length(cols))
    los <- map2_chr(rows[seq_len(len)], cols[seq_len(len)], ~seats[.x, .y])
  } else if (direction == "S") {
    los <- seats[row:dim[1] %>% .[. != row], col]
  } else if (direction == "SW") {
    rows <- row:dim[1] %>% .[. != row]
    cols <- col:1 %>% .[. != col]
    len <- min(length(rows), length(cols))
    los <- map2_chr(rows[seq_len(len)], cols[seq_len(len)], ~seats[.x, .y])
  } else if (direction == "W") {
    los <- seats[row, col:1 %>% .[. != col]]
  } else if (direction == "NW") {
    rows <- row:1 %>% .[. != row]
    cols <- col:1 %>% .[. != col]
    len <- min(length(rows), length(cols))
    los <- map2_chr(rows[seq_len(len)], cols[seq_len(len)], ~seats[.x, .y])
  }
  # first visible chair in line-of-site
  los[los != FLOOR][1]
}

visible_occupied_neighbors <- function(seats, row, col) {
  directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  visible <- map_chr(directions, ~visible_neighbor(seats, row, col, .))
  sum(visible == OCCUPIED, na.rm = TRUE)
}

update_seats2 <- function(seats) {
  new_seats <- seats
  for (row in seq_len(nrow(seats))) {
    for (col in which(seats[row, ] != FLOOR)) {
      seat <- seats[row, col]
      neighbors <- visible_occupied_neighbors(seats, row, col)
      if (seat == EMPTY && neighbors == 0) {
        new_seats[row, col] <- OCCUPIED
      } else if (seat == OCCUPIED && neighbors >= 5) {
        new_seats[row, col] <- EMPTY
      }
    }
  }
  new_seats
}

# * test neighbor visibility ====
example_text <- ".......#.
  ...#.....
  .#.......
  .........
  ..#L....#
  ....#....
  .........
  #........
  ...#....." %>% 
  str_split("\n") %>% unlist() %>% trimws()
example_map <- build_seat_map(example_text)
expect_equal(visible_occupied_neighbors(example_map, 5, 4), 8)
expect_equal(visible_occupied_neighbors(example_map, 5, 5), 2)

# * test stable seating ====
example_text <- "L.LL.LL.LL
  LLLLLLL.LL
  L.L.L..L..
  LLLL.LL.LL
  L.LL.LL.LL
  L.LLLLL.LL
  ..L.L.....
  LLLLLLLLLL
  L.LLLLLL.L
  L.LLLLL.LL" %>% 
  str_split("\n") %>% unlist() %>% trimws()
example_map <- build_seat_map(example_text)
stable_seats <- find_stable_seating(example_map, update_seats2)
expect_equal(sum(stable_seats == OCCUPIED), 26)

# * sort real seat map ====
microbenchmark::microbenchmark(
  stable_seats <- find_stable_seating(seat_map, update_seats2),
  times = 1L
)
# ran in ~5m
stable_seats
sum(stable_seats == OCCUPIED)
# 2091


# * more efficient vesion with cached neighbors ====

visible_neighbor_coord <- function(seats, row, col, direction) {
  dim <- dim(seats)
  if (direction == "N") {
    rows <- row:1 %>% .[. != row]
    cols <- rep(col, length(rows))
  } else if (direction == "NE") {
    rows <- row:1 %>% .[. != row]
    cols <- col:dim[2] %>% .[. != col]
  } else if (direction == "E") {
    cols <- col:dim[2] %>% .[. != col]
    rows <- rep(row, length(cols))
  } else if (direction == "SE") {
    rows <- row:dim[1] %>% .[. != row]
    cols <- col:dim[2] %>% .[. != col]
  } else if (direction == "S") {
    rows <- row:dim[1] %>% .[. != row]
    cols <- rep(col, length(rows))
  } else if (direction == "SW") {
    rows <- row:dim[1] %>% .[. != row]
    cols <- col:1 %>% .[. != col]
  } else if (direction == "W") {
    cols <- col:1 %>% .[. != col]
    rows <- rep(row, length(cols))
  } else if (direction == "NW") {
    rows <- row:1 %>% .[. != row]
    cols <- col:1 %>% .[. != col]
  }
  # coordinate of visible chair in line-of-site
  len <- min(length(rows), length(cols))
  rows <- rows[seq_len(len)]
  cols <- cols[seq_len(len)]
  los <- map2_chr(rows, cols, ~seats[.x, .y])
  visible <- which(los != FLOOR)[1]
  c(rows[visible], cols[visible])
}

build_neighbor_map <- function(seats) {
  dim <- dim(seats)
  directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  nearest_neighbors <- array(list(), dim = dim)
  for (row in seq_len(dim[1])) {
    for (col in seq_len(dim[2])) {
      neighbor_coords <- map(directions, ~visible_neighbor_coord(seats, row, col, .x))
      nearest_neighbors[row, col][[1]] <- list(
        rows = map_dbl(neighbor_coords, ~.x[1]) %>% na.omit() %>% as.numeric(),
        cols = map_dbl(neighbor_coords, ~.x[2]) %>% na.omit() %>% as.numeric()
      )
    }
  }
  nearest_neighbors
}

update_seats3 <- function(seats, neighbor_map) {
  new_seats <- seats
  for (row in seq_len(nrow(seats))) {
    for (col in which(seats[row, ] != FLOOR)) {
      seat <- seats[row, col]
      neighbor_coords <- neighbor_map[row, col][[1]]
      visible_seats <- map2_chr(
        neighbor_coords$rows, neighbor_coords$cols,
        ~seats[.x, .y]
      )
      neighbors <- sum(visible_seats == OCCUPIED)
      if (seat == EMPTY && neighbors == 0) {
        new_seats[row, col] <- OCCUPIED
      } else if (seat == OCCUPIED && neighbors >= 5) {
        new_seats[row, col] <- EMPTY
      }
    }
  }
  new_seats
}


# * sort seat map (optimized) ====
# cached map of nearest neighbor seats
neighbor_map <- build_neighbor_map(seat_map)
microbenchmark::microbenchmark(
  stable_seats <- find_stable_seating(seat_map, update_seats3, neighbor_map = neighbor_map),
  times = 1L
)
# ran in ~28s
stable_seats
sum(stable_seats == OCCUPIED)
