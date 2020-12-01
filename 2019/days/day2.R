# DAY 2
library(glue)
library(magrittr)
library(rlang)
library(stringr)
library(testthat)

# read input data
ints_raw <- readLines(file.path("data-raw", "day2-part1.txt"), warn = FALSE)


# part 1 ====
parse_integers <- function(x) {
  str_split(x, ",")[[1]] %>% as.numeric()
}
get_val <- function(ints, index) {
  ints[index + 1]
}
set_val <- function(ints, index, val) {
  ints[index + 1] <- val
  ints
}
get_ref_val <- function(ints, index) {
  ints[get_val(ints, index) + 1]
}
set_ref_val <- function(ints, index, val) {
  ints[get_val(ints, index) + 1] <- val
  ints
}
calculate_op <- function(ints, index, op) {
  output_val <- {{op}}(get_ref_val(ints, index + 1), 
                       get_ref_val(ints, index + 2))
  set_ref_val(ints, index + 3, output_val)
}
intcode <- function(ints, index = 0) {
  opcode <- get_val(ints, index)
  switch(
    as.character(opcode),
    "1" = intcode(calculate_op(ints, index, op = `+`), index + 4),
    "2" = intcode(calculate_op(ints, index, op = `*`), index + 4),
    "99" = ints,
    stop(glue("unexpected opcode: '{val}' at index {index}",
                   val = get_ref_val(ints, index), index = index))
  )
}

# test cases
expect_equal(intcode(parse_input("1,0,0,0,99")), c(2,0,0,0,99))
expect_equal(intcode(parse_input("2,3,0,3,99")), c(2,3,0,6,99))
expect_equal(intcode(parse_input("2,4,4,5,99,0")), c(2,4,4,5,99,9801))
expect_equal(intcode(parse_input("1,1,1,4,99,5,6,0,99")), c(30,1,1,4,2,5,6,0,99))

# replace position 1 with the value 12 and replace position 2 with the value 2
ints <- parse_input(ints_raw)
ints <- set_val(ints, 1, 12)
ints <- set_val(ints, 2, 2)
ints_output <- intcode(ints)
get_val(ints_output, 0)


# part 2 ====
goal <- 19690720
nouns <- c(0:99)
verbs <- c(0:99)
ints_input <- parse_input(ints_raw)

seek_goal <- function(input, goal, nouns, verbs) {
  for (noun in nouns) {
    for (verb in verbs) {
      ints <- ints_input
      ints <- set_val(ints, 1, noun)
      ints <- set_val(ints, 2, verb)
      ints_output <- intcode(ints)
      if (get_val(ints_output, 0) == goal) {
        message("goal hit!")
        return(c("noun" = noun, "verb" = verb))
      }
    }
  }
  message("goal not found...")
  c("noun" = NULL, "verb" = NULL)
}

results <- seek_goal(ints_input, goal = goal, nouns = nouns, verbs = verbs)
results
100 * results["noun"] + results["verb"]


