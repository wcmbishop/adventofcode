# DAY 6
library(rlang)
library(dplyr)
library(magrittr)
library(purrr)
library(stringr)
library(testthat)
library(tidyr)
library(tibble)

# read input data
input <- readLines(file.path("data-raw", "day6.txt"), warn = FALSE)


# part 1 ====
parse_orbits <- function(input) {
  tibble(input = input) %>% 
    tidyr::separate(input, into = c("orbitee", "orbiter"), 
                    sep = "\\)", remove = FALSE) %>% 
    mutate(level = NA_real_)
}
count_orbits <- function(orbits, around = "COM", orbit_level = 0) {
  # recursive orbit counting
  direct_orbits <- filter(orbits, orbitee == around)
  if (nrow(direct_orbits) == 0) {
    return(orbit_level)
  } else {
    n_indirect <- purrr::map_dbl(
      direct_orbits$orbiter, 
      ~count_orbits(orbits, around = .x, orbit_level = orbit_level + 1))
  }
  sum(n_indirect) + orbit_level
}

# test cases
test_input <- c(
  "COM)B",
  "B)C",
  "C)D",
  "D)E",
  "E)F",
  "B)G",
  "G)H",
  "D)I",
  "E)J",
  "J)K",
  "K)L")
expect_equal(count_orbits("COM", orbits = parse_orbits(test_input)), 42)

# input
orbits <- parse_orbits(input)
count_orbits(orbits, around = "COM")

# part 2 ====
define_orbit_levels <- function(orbits, around = "COM", level = 1) {
  orbits$level[orbits$orbitee == around] <- level
  direct_orbits <- filter(orbits, orbitee == around)
  for (i in seq_len(nrow(direct_orbits))) {
    orbits <- define_orbit_level(orbits, around = direct_orbits$orbiter[i],
                                 level = level + 1)
  }
  orbits
}
filter_orbit_path <- function(orbits, around = "COM", orbiter) {
  # filter to orbits that are on the "path" to the given orbiter
  parent <- filter(orbits, orbiter == !!orbiter)
  if (parent$orbitee == around) {
    return(parent)
  } else {
    orbit_path <- dplyr::bind_rows(
      parent,
      filter_orbit_path(orbits, around = around, orbiter = parent$orbitee)
    )
  }
  orbit_path
}
count_orbit_transfers <- function(orbits, center = "COM", source, dest) {
  # orbit paths from 
  source_path <- filter_orbit_path(orbits, around = center, orbiter = source)
  dest_path <- filter_orbit_path(orbits, around = center, orbiter = dest)
  # closest shared node
  closest_shared_orbit <- source_path %>% 
    semi_join(dest_path, by = "orbitee") %>% 
    top_n(1, wt = level)
  # sum orbit levels from shared node to source/dest orbit nodes
  source_orbit <- orbits %>% filter(orbiter == source)
  dest_orbit <- orbits %>% filter(orbiter == dest)
  (dest_orbit$level - closest_shared_orbit$level) + 
    (source_orbit$level - closest_shared_orbit$level)
}

# test cases
test_input <- c(
  "COM)B",
  "B)C",
  "C)D",
  "D)E",
  "E)F",
  "B)G",
  "G)H",
  "D)I",
  "E)J",
  "J)K",
  "K)L",
  "K)YOU",
  "I)SAN"
  )
orbits <- parse_orbits(test_input)
orbits <- define_orbit_levels(orbits)
expect_equal(count_orbit_transfers(orbits, source = "YOU", dest = "SAN"), 4)

# input
orbits <- parse_orbits(input)
orbits <- define_orbit_levels(orbits)
count_orbit_transfers(orbits, source = "YOU", dest = "SAN")

