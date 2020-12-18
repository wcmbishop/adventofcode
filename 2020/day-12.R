library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
instructions <- file.path("data-raw", "day-12", "navigation.txt") %>% 
  readLines()
# turns stay on 90 degree directions
instructions[str_detect(instructions, "L|R")] %>% unique()


# part 1 ====
move <- function(direction, distance) {
  step <- switch(
    direction,
    N = c(0, 1),
    E = c(1, 0),
    S = c(0, -1),
    W = c(-1, 0)
  )
  step * distance
}
turn <- function(facing, turn, degrees) {
  directions <- c("E", "S", "W", "N")  # ordered clock-wise (R)
  n_turns <- ifelse(turn == "R", degrees / 90, -degrees / 90)
  dir_index <- which(directions == facing)
  directions[(dir_index + n_turns - 1) %% length(directions) + 1]
}

# * test functions ==== 
# move
expect_equal(move("E", 5), c(5, 0))
expect_equal(move("S", 3), c(0, -3))
expect_equal(move("W", 2), c(-2, 0))
# turn
expect_equal(turn("E", "L", 90), "N")
expect_equal(turn("S", "R", 270), "E")
expect_equal(turn("N", "L", 90), "W")

# * run navigation instructions ====
loc <- c(0, 0)
dir <- "E"
for (instruction in instructions) {
  cmd <- str_match(instruction, "([A-Z])[0-9]+")[1, 2] 
  amt <- str_match(instruction, ".([0-9]+)")[1, 2] %>% as.numeric()
  if (cmd %in% c("L", "R")) {
    dir <- turn(dir, turn = cmd, degrees = amt)
  } else if (cmd == "F") {
    loc <- loc + move(dir, amt)
  } else {
    loc <- loc + move(cmd, amt) 
  }
}
sum(abs(loc))


# part 2 ====
move_waypoint <- function(waypoint, direction, distance) {
  step <- switch(
    direction,
    N = c(0, 1),
    E = c(1, 0),
    S = c(0, -1),
    W = c(-1, 0)
  )
  waypoint + (step * distance)
}
rotate_waypoint <- function(waypoint, turn, degrees) {
  theta <- ifelse(turn == "R", -degrees, degrees) * (pi/180)
  c(
    cos(theta)*waypoint[1] - sin(theta)*waypoint[2],
    sin(theta)*waypoint[1] + cos(theta)*waypoint[2]
  )
}
move_ship <- function(loc, waypoint, times) {
  loc + (waypoint * times)
}

# * test functions ====
expect_equal(move_waypoint(c(10, 1), "N", 3), c(10, 4))
expect_equal(move_ship(c(0, 0), c(10, 1), 10), c(100, 10))
expect_equal(rotate_waypoint(c(10, 4), "R", 90), c(4, -10))

# * run navigation ====
loc <- c(0, 0)
waypoint <- c(10, 1)
for (instruction in instructions) {
  cmd <- str_match(instruction, "([A-Z])[0-9]+")[1, 2] 
  amt <- str_match(instruction, ".([0-9]+)")[1, 2] %>% as.numeric()
  if (cmd %in% c("L", "R")) {
    waypoint <- rotate_waypoint(waypoint, turn = cmd, degrees = amt)
  } else if (cmd == "F") {
    loc <- move_ship(loc, waypoint, amt)
  } else {
    waypoint <- move_waypoint(waypoint, cmd, amt) 
  }
}
sum(abs(loc))
