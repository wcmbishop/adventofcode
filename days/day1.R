# DAY 1 
library(magrittr)
library(purrr)
module_masses <- readLines(
  file.path("data-raw", "day1-part1.txt"), warn = FALSE) %>% 
  as.numeric()

# part 1 ====
launch_fuel <- function(mass) {
  floor(mass / 3) - 2
}
fuel <- launch_fuel(module_masses) %>% sum()
fuel


# part 2 ====
recursive_launch_fuel <- function(mass) {
  fuel <- launch_fuel(mass)
  ifelse(fuel > 0, fuel + recursive_launch_fuel(fuel), 0)
}
fuel <- map_dbl(module_masses, recursive_launch_fuel) %>% sum()
fuel
