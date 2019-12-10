# DAY 5
library(rlang)
library(dplyr)
library(magrittr)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
source("R/intcode.R")

# read input data
ints <- readLines(file.path("data-raw", "day5.txt"), warn = FALSE)

# part 1 ====
# test cases
expect_equal(Intcode$new("1,0,0,0,99")$run()$memory, c(2,0,0,0,99))
expect_equal(Intcode$new("2,3,0,3,99")$run()$memory, c(2,3,0,6,99))
expect_equal(Intcode$new("2,4,4,5,99,0")$run()$memory, c(2,4,4,5,99,9801))
expect_equal(Intcode$new("1,1,1,4,99,5,6,0,99")$run()$memory, c(30,1,1,4,2,5,6,0,99))
expect_equal(Intcode$new("1101,100,-1,4,0")$run()$memory, c(1101,100,-1,4,99))

intcode <- Intcode$new(state = ints, input = 1)
intcode$run()
intcode$output

# part 2 ====

# test cases 
int_ex <- "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
expect_equal(Intcode$new(int_ex, input = 1)$run()$output, 999)
expect_equal(Intcode$new(int_ex, input = 8)$run()$output, 1000)
expect_equal(Intcode$new(int_ex, input = 9)$run()$output, 1001)

intcode <- Intcode$new(state = ints, input = 5)
intcode$run()
intcode$output

