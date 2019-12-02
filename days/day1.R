# DAY 1 
library(magrittr)
library(purrr)


# part 1 ====
launch_fuel <- function(mass) {
  floor(mass / 3) - 2
}
module_masses <- readLines(
  file.path("data-raw", "day1-part1.txt"), warn = FALSE) %>% 
  as.numeric()
total_fuel <- launch_fuel(module_masses) %>% 
  sum()
total_fuel


# part 2 ====
recursive_launch_fuel <- function(mass) {
  fuel <- launch_fuel(mass)
  if (fuel > 0) {
    extra_fuel <- recursive_launch_fuel(fuel)
    return(fuel + extra_fuel)
  } else {
    return(0)
  }
}
total_recursive_fuel <- purrr::map_dbl(
  module_masses, recursive_launch_fuel) %>% 
  sum()
total_recursive_fuel


