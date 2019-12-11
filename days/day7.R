# DAY 7
library(rlang)
library(dplyr)
library(gtools)
library(magrittr)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
source("R/intcode.R")

# read input data
ints <- readLines(file.path("data-raw", "day7.txt"), warn = FALSE)


# part 1 ====
run_amplifiers <- function(ints, phases, input = 0) {
  suppressMessages({
    amp_a <- Intcode$new(state = ints, inputs = c(phases[1], input))$run()
    amp_b <- Intcode$new(state = ints, inputs = c(phases[2], amp_a$outputs))$run()
    amp_c <- Intcode$new(state = ints, inputs = c(phases[3], amp_b$outputs))$run()
    amp_d <- Intcode$new(state = ints, inputs = c(phases[4], amp_c$outputs))$run()
    amp_e <- Intcode$new(state = ints, inputs = c(phases[5], amp_d$outputs))$run()
  })
  amp_e$output
}

# test cases
expect_equal(
  run_amplifiers(c("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"), 
                 phases = c(4,3,2,1,0)), 43210
)
expect_equal(
  run_amplifiers(c("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"), 
                 phases = c(0,1,2,3,4)), 54321
)
expect_equal(
  run_amplifiers(c("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"), 
                 phases = c(1,0,4,3,2)), 65210
)

# find optimum phases
phases <- c(0:4)
phases_matrix <- gtools::permutations(length(phases), length(phases), v = phases)
# find max amp output
all_phases <- tibble(
  phases = purrr::map(seq_len(nrow(phases_matrix)), ~phases_matrix[.x, ])
  ) %>% 
  mutate(amp_output = purrr::map_dbl(phases, ~run_amplifiers(ints, .x)))
all_phases %>% 
  top_n(1, amp_output)


# part 2 ====
run_feedback_amplifiers <- function(ints, phases, input = 0) {
  # initial run
  suppressMessages({
    amp_a <- Intcode$new(state = ints, inputs = c(phases[1], input))$run()
    amp_b <- Intcode$new(state = ints, inputs = c(phases[2], amp_a$outputs))$run()
    amp_c <- Intcode$new(state = ints, inputs = c(phases[3], amp_b$outputs))$run()
    amp_d <- Intcode$new(state = ints, inputs = c(phases[4], amp_c$outputs))$run()
    amp_e <- Intcode$new(state = ints, inputs = c(phases[5], amp_d$outputs))$run()
  })
  if (amp_e$is_complete)
    return(amp_e$output)
  while (TRUE) {
    suppressMessages({
      amp_a$add_inputs(amp_e$outputs)$reset_outputs()$run()
      amp_b$add_inputs(amp_a$outputs)$reset_outputs()$run()
      amp_c$add_inputs(amp_b$outputs)$reset_outputs()$run()
      amp_d$add_inputs(amp_c$outputs)$reset_outputs()$run()
      amp_e$add_inputs(amp_d$outputs)$reset_outputs()$run()
    })
    if (amp_e$is_complete) 
      break
  }
  amp_e$output
}

# test cases 
expect_equal(
  run_feedback_amplifiers(c("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"), 
                          phases = c(9,8,7,6,5)), 139629729
)
expect_equal(
  run_feedback_amplifiers(c("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"), 
                          phases = c(9,7,8,5,6)), 18216
)

# find max amp output
phases <- c(5:9)
phases_matrix <- gtools::permutations(length(phases), length(phases), v = phases)
all_phases <- tibble(
  phases = purrr::map(seq_len(nrow(phases_matrix)), ~phases_matrix[.x, ])
  ) %>% 
  mutate(amp_output = purrr::map_dbl(phases, ~run_feedback_amplifiers(ints, .x)))
all_phases %>% 
  top_n(1, amp_output)

top_phases$amp_output