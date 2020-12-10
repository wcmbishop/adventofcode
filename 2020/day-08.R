library(dplyr)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
instructions <- file.path("data-raw", "day-08", "instructions.txt") %>% 
  readLines()

# part 1 ====
parse_instruction <- function(x) {
  split <- str_split(x, pattern = " ")[[1]]
  list(op = split[1], arg = as.numeric(split[2]))
}

accumulator <- 0
runs <- rep.int(0, length(instructions))
index <- 1

# loop through instructions
while (TRUE) {
  runs[index] <- runs[index] + 1
  if (runs[index] > 1) {
    print(paste("accumulator:", accumulator))
    break
  }
  
  instr <- parse_instruction(instructions[index])
  if (instr$op == "acc") {
    accumulator <- accumulator + instr$arg
    index <- index + 1
  } else if (instr$op == "jmp") {
    index <- index + instr$arg
  } else {
    index <- index + 1
  }
}


# part 2 ====
run_program <- function(instructions) {
  accumulator <- 0
  runs <- rep.int(0, length(instructions))
  index <- 1
  finished <- FALSE
  while (TRUE) {
    if (index > length(instructions)) {
      print("program complete!")
      finished <- TRUE
      break
    }
    runs[index] <- runs[index] + 1
    if (runs[index] > 1) {
      print(paste("infinite loop! accumulator:", accumulator))
      break
    }
    
    instr <- parse_instruction(instructions[index])
    if (instr$op == "acc") {
      accumulator <- accumulator + instr$arg
      index <- index + 1
    } else if (instr$op == "jmp") {
      index <- index + instr$arg
    } else {
      index <- index + 1
    }
  }
  
  list(accumulator = accumulator, runs = runs, finished = finished)
}

# find which instructions were run
results <- run_program(instructions)
candidates <- which(results$runs > 0 & str_detect(instructions, "(jmp)|(nop)"))
# search for instruction changes to find a program that finishes
for (candidate in candidates) {
  new_insructions <- instructions
  new_op <- ifelse(
    str_detect(instructions[candidate], "jmp"), "nop", "jmp")
  new_insructions[candidate] <- str_replace(
    new_insructions[candidate], pattern = "(jmp)|(nop)", replacement = new_op)
  results <- run_program(new_insructions)
  if (results$finished) {
    print(paste("accumulator:", results$accumulator))
    break 
  }
}

