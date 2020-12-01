# DAY 10
library(dplyr)
library(ggplot2)
library(purrr)
library(rlang)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)


# part 1 ====

# function definitions
parse_input <- function(input) {
  result <- tibble(x = integer(), y = integer(), asteroid = logical())
  for (i in seq(input)) {
    row <- input[i]
    for (j in seq_len(nchar(row))) {
      cell <- substr(row, j, j)
      result <- result %>% add_row(x = j - 1, y = i - 1, asteroid = cell == "#")
    }
  }
  result %>% 
    mutate(id = seq_len(n()))
}
calc_angle <- function(loc1, loc2) {
  vec <- loc2 - loc1
  atan2(-vec[2], vec[1])
}
asteroid_angles <- function(asteroids, root_id) {
  root <- filter(asteroids, id == root_id)
  asteroids %>% 
    filter(id != root_id) %>% 
    mutate(root_id = root_id,
           angle = map2_dbl(x, y, 
                        ~calc_angle(loc1 = c(root$x, root$y),
                                    loc2 = c(.x, .y)))
    )
}
calc_all_asteroid_angles <- function(asteroids) {
  asteroids <- filter(asteroids, asteroid == TRUE)
  angles_list <- vector("list", length = nrow(asteroids))
  for (i in seq_along(asteroids$id)) {
    angles_list[[i]] = asteroid_angles(asteroids, root_id = asteroids$id[i])
  }
  dplyr::bind_rows(angles_list)
}
asteroid_los <- function(asteroids) {
  angles <- calc_all_asteroid_angles(asteroids)
  angles %>% 
    group_by(id = root_id) %>% 
    summarise(los_count = n_distinct(angle)) %>% 
    inner_join(asteroids, by = "id")
}
find_monitoring_station <- function(asteroids) {
  los <- asteroid_los(asteroids)
  los %>% filter(los_count == max(los_count))
}

# test cases
input <- c(
  ".#..#",
  ".....",
  "#####",
  "....#",
  "...##"
  )
station <- find_monitoring_station(parse_input(input))
expect_equal(c(station$x, station$y), c(3, 4))
expect_equal(station$los_count, 8)

input <- c(
  "......#.#.",
  "#..#.#....",
  "..#######.",
  ".#.#.###..",
  ".#..#.....",
  "..#....#.#",
  "#..#....#.",
  ".##.#..###",
  "##...#..#.",
  ".#....####"
)
station <- find_monitoring_station(parse_input(input))
expect_equal(c(station$x, station$y), c(5, 8))
expect_equal(station$los_count, 33)


# real input
input <- readLines(file.path("data-raw", "day10.txt"), warn = FALSE)
asteroids <- parse_input(input)
station <- find_monitoring_station(asteroids)
station


# part 2 ====

# function definitions
calculate_distance <- function(loc1, loc2) {
  vec <- loc2 - loc1
  sqrt(vec[1]^2 + vec[2]^2)
}
convert_angle_to_laser_angle <- function(angle) {
  # convert +/- angles to positive 0-2pi
  laser_angle <- if_else(angle < 0, 2*pi + angle, angle)
  # rotate angle 90 degrees counter-clockwise
  laser_angle <- laser_angle - pi/2
  laser_angle <- if_else(laser_angle < 0, 2*pi + laser_angle, laser_angle)
  # flip rotation to be clockwise
  laser_angle <- 2*pi - laser_angle
  laser_angle <- if_else(laser_angle == 2*pi, 0, laser_angle)
  laser_angle
}

# real input
input <- readLines(file.path("data-raw", "day10.txt"), warn = FALSE)
asteroids <- parse_input(input)
station <- find_monitoring_station(asteroids)
station_los <- asteroid_angles(asteroids, station$id) %>% 
  filter(asteroid == TRUE) %>% 
  mutate(root_x = station$x,
         root_y = station$y,
         distance = purrr::pmap_dbl(
           list(x = x, y = y, root_x = root_x, root_y = root_y),
           function(x, y, root_x, root_y) calculate_distance(c(x, y), c(root_x, root_y))))
# calculate laser angles, then order by angle and distance
station_los <- station_los %>% 
  mutate(laser_angle = convert_angle_to_laser_angle(angle)) %>% 
  arrange(laser_angle, distance) %>% 
  group_by(laser_angle) %>% 
  mutate(angle_order = group_indices(),
         distance_order = seq_len(n())) %>% 
  ungroup() %>% 
  arrange(distance_order, angle_order) %>% 
  mutate(laser_order = seq_len(n()))
# find 200th asteroid
asteroid_200 <- station_los %>% slice(200)
asteroid_200
asteroid_200$x*100 + asteroid_200$y


