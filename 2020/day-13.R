library(dplyr)
library(furrr)
library(glue)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
input <- file.path("data-raw", "day-13", "buses.txt") %>% 
  readLines()
min_timestamp <- as.numeric(input[1])
bus_ids <- str_split(input[2], pattern = ",")[[1]] %>% 
  as.numeric() %>% 
  na.omit() %>% 
  as.numeric()


# part 1 ====
# count forward in time until a bus_id is a match
timestamp <- min_timestamp
repeat {
  bus_arrives <- (timestamp %% bus_ids) == 0
  if (any(bus_arrives)) {
    message(
      glue("bus arrives!\nwait time: {wait}\nbus id: {bus_id}",
           wait = timestamp - min_timestamp,
           bus_id = bus_ids[bus_arrives]
      )
    )
    break
  } 
  timestamp <- timestamp + 1
}


# part 2 ====
# recursive search
find_time <- function(ids, n_guesses, start = 0, iterate = TRUE, print = FALSE) {
  if (print) 
    message(glue("{now} - search start: {start}", now = Sys.time()))
  times <- ids[1] * (seq_len(n_guesses) + start)
  filter_indices <- which(!is.na(ids))[-1]
  filtered_ids <- ids[filter_indices]
  delays <- filter_indices - 1
  match_lists <- map2(filtered_ids, delays, function(id = .x, delay = .y) {
    times[((times + delay) %% id) == 0]
  })
  matches <- reduce(match_lists, intersect)
  if (length(matches) > 0) {
    match <- matches[1]
  } else if (iterate) {
    match <- find_time(
      ids, 
      n_guesses = n_guesses, 
      start = start + n_guesses,
      iterate = iterate,
      print = print
    )
  } else {
    match <- NA_real_
  }
  match
}

# * test function ==== 
example_ids <- str_split("7,13,x,x,59,x,31,19", pattern = ",")[[1]] %>% as.numeric()
expect_equal(find_time(example_ids, 10000), 1068781)
example_ids <- str_split("17,x,13,19", pattern = ",")[[1]] %>% as.numeric()
expect_equal(find_time(example_ids, 1000), 3417)


# * find time with real input ====
# > 100000000000000
bus_ids <- str_split(input[2], pattern = ",")[[1]] %>% as.numeric() 
# this takes too long!
# find_time(bus_ids, 1e6, start = 100000000000000, iterate = TRUE)

# take incremental multiplier steps
indices <- which(!is.na(bus_ids))
ids <- bus_ids[indices]
delays <- indices - 1
time <- 0
step <- 1
for (i in seq_along(ids)) {
  remainders <- (time + step * seq(1, ids[i]) + delays[i]) %% ids[i]
  time <- time + step * first(which(remainders == 0))
  step <- step * ids[i]
}
time
as.character(time)
