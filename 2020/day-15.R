library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)


# part 1 ====
numbers_game <- function(starting_numbers, limit = 2020) {
  memory <- list()
  for (i in seq_len(limit)) {
    if (i <= length(starting_numbers)) {
      number <- as.character(starting_numbers[i])
      memory[[number]] <- i
      next
    } 
    number <- ifelse(
      length(memory[[number]]) == 1, 
      0, 
      memory[[number]][1] - memory[[number]][2]
    ) %>% 
      as.character()
    memory[[number]] <- c(i, memory[[number]][1])
  }
  as.integer(number)
}

# test inputs
expect_equal(numbers_game(c(1, 3, 2)), 1)
expect_equal(numbers_game(c(2, 1, 3)), 10)
expect_equal(numbers_game(c(1, 2, 3)), 27)

# real input
numbers_game(c(0,1,4,13,15,12,16))


# part 2 ====
# test inputs
expect_equal(numbers_game(c(0, 3, 6), limit = 30000000), 175594)
expect_equal(numbers_game(c(2, 1, 3)), 10)
expect_equal(numbers_game(c(1, 2, 3)), 27)

# real input
numbers_game(c(0,1,4,13,15,12,16))
