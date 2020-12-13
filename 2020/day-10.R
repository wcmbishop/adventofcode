library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
adapters <- file.path("data-raw", "day-10", "adapters.txt") %>% 
  readLines() %>% 
  as.numeric()

# part 1 ====
# test example
example <- c(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19,
             38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)
example <- example[order(example)]
diffs <- diff(c(0, example), lag = 1) %>% c(., 3)
expect_equal(sum(diffs == 1), 22)
expect_equal(sum(diffs == 3), 10)

adapters <- adapters[order(adapters)]
diffs <- diff(c(0, adapters), lag = 1) %>% c(., 3)
sum(diffs == 1) * sum(diffs == 3)


# part 2 ====
compatible_adapters <- function(source, adapters) {
  adapters[adapters > source & (adapters - source) <= 3]
}
# recursive brute force method
count_chains <- function(source, adapters) {
  if (length(adapters) == 0) return(1)
  options <- compatible_adapters(source, adapters)
  if (length(options) == 0) return(0)
  sum(map_dbl(options, ~count_chains(., adapters[adapters > .])))
}

# short example
example <- c(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
example <- example[order(example)]
ex_adapters <- c(example, max(example) + 3)
expect_equal(count_chains(0, ex_adapters), 8)
# longer example
example <- c(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19,
             38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)
example <- example[order(example)]
ex_adapters <- c(example, max(example) + 3)
expect_equal(count_chains(0, ex_adapters), 19208)

# real input - brute force takes too long!
adapters <- adapters[order(adapters)]
my_adapters <- c(adapters, max(adapters) + 3)
# count_chains(0, adapters = my_adapters)

# examing adapter differences
# - only runs of 1 and 3 -- 1 has runs of 1,2,3,4
diffs <- diff(c(0, adapters), lag = 1) %>% c(., 3)
runs <- rle(diffs)
unique(runs$values)
unique(runs$lengths)

# define combinations for different run lengths of 1 
# 0 - A --- X              # run of 1 - 1 combo
# 0 - A - B --- X          # run of 2 - 2 combos
# 0 - A - B - C --- X      # run of 3 - 4 combos
# 0 - A - B - C - D --- X  # run of 4 - 7 combos

runs_df <- tibble(
  diff = runs$values,
  length = runs$lengths,
  combos = if_else(
    diff == 3, 1,
    case_when(
      length == 1 ~ 1,
      length == 2 ~ 2,
      length == 3 ~ 4,
      length == 4 ~ 7
    )
  )
)
prod(runs_df$combos)
