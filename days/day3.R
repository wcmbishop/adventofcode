# DAY 2
library(dplyr)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)
library(testthat)

# read input data
input_raw <- readLines(file.path("data-raw", "day3-part1.txt"), warn = FALSE)


# part 1 ====
wire_steps <- function(code) {
  # calculate tibble of x/y wire steps from direction code
  direction <- str_sub(code, 1, 1)
  n_steps <- str_sub(code, 2, -1) %>% as.numeric()
  x_step <- switch(direction, "R" = 1, "L" = -1, 0)
  y_step <- switch(direction, "U" = 1, "D" = -1, 0)
  tibble(x_step = rep(x_step, n_steps), 
         y_step = rep(y_step, n_steps))
}
build_wire_maps <- function(input) {
  # generate wire maps (tibble of x/y coordinates)
  directions <- str_split(input, ",")
  wire_maps <- vector("list", length = length(directions))
  for (wire_num in seq_along(directions)) {
    wire_maps[[wire_num]] <- map_dfr(
      directions[[wire_num]], wire_steps, .id = "run_id") %>% 
      mutate(order = seq_len(n()),
             x = cumsum(x_step),
             y = cumsum(y_step),
             distance = abs(x) + abs(y))
  }
  wire_maps
} 
find_closest_intersection <- function(input) {
  wire_maps <- build_wire_maps(input)
  # filter to closest intersection
  wire_maps[[1]] %>% 
    semi_join(wire_maps[[2]], by = c("x", "y")) %>% 
    top_n(1, desc(distance)) %>% 
    select(x, y, distance)
}

# test cases
expect_equal(
  find_closest_intersection(
    c("R8,U5,L5,D3",
      "U7,R6,D4,L4"))$distance, 
  6
)
expect_equal(
  find_closest_intersection(
    c("R75,D30,R83,U83,L12,D49,R71,U7,L72",
      "U62,R66,U55,R34,D71,R55,D58,R83"))$distance, 
  159
)
expect_equal(
  find_closest_intersection(
    c("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))$distance, 
  135
)

find_closest_intersection(input_raw)


# part 2 ====

find_shortest_signal <- function(input) {
  wire_maps <- build_wire_maps(input)
  # filter to shortest signal steps
  wire_maps[[1]] %>% select(x, y, steps1 = order) %>% 
    inner_join(wire_maps[[2]] %>% select(x, y, steps2 = order),
               by = c("x", "y")) %>% 
    mutate(signal_steps = steps1 + steps2) %>% 
    top_n(1, desc(signal_steps))
}

# test cases
expect_equal(
  find_shortest_signal(
    c("R8,U5,L5,D3",
      "U7,R6,D4,L4"))$signal_steps, 
  30
)
expect_equal(
  find_shortest_signal(
    c("R75,D30,R83,U83,L12,D49,R71,U7,L72",
      "U62,R66,U55,R34,D71,R55,D58,R83"))$signal_steps, 
  610
)
expect_equal(
  find_shortest_signal(
    c("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))$signal_steps, 
  410
)

find_shortest_signal(input_raw)
