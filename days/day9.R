# DAY 9
library(dplyr)
library(ggplot2)
library(purrr)
library(rlang)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)
source(file.path("R", "intcode.R"))
options("expressions" = 10000)

# read input data
input <- readLines(file.path("data-raw", "day9.txt"), warn = FALSE)


# part 1 ====
# test cases
expect_equal(Intcode$new("2,3,0,3,99")$run()$memory, c(2,3,0,6,99))
expect_equal(Intcode$new("2,4,4,5,99,0")$run()$memory, c(2,4,4,5,99,9801))
expect_equal(Intcode$new("1,1,1,4,99,5,6,0,99")$run()$memory, c(30,1,1,4,2,5,6,0,99))
expect_equal(Intcode$new("1101,100,-1,4,0")$run()$memory, c(1101,100,-1,4,99))
expect_equal(Intcode$new("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")$run()$outputs,
             c(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))
expect_equal(Intcode$new("1102,34915192,34915192,7,4,7,99,0")$run()$outputs,
             1219070632396864)
expect_equal(Intcode$new("104,1125899906842624,99")$run()$outputs,
             1125899906842624)

intcode <- Intcode$new(input, inputs = 1)
intcode$run()
intcode$outputs


# part 2 ====
intcode <- Intcode$new(input, inputs = 2)
intcode$run()
intcode$outputs

