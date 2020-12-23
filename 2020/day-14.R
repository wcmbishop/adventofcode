library(bit64)
library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
input <- file.path("data-raw", "day-14", "program.txt") %>% 
  readLines()


# part 1 ====
# fine memory address range
input[str_detect(input, "mem")] %>% 
  str_match("\\[(\\d+)\\]") %>% 
  .[, 2] %>% 
  as.numeric() %>% 
  range()

memory <- rep(0L, 2^16) %>% as.integer64()
for (i in seq_along(input)) {
  instr <- input[i]
  if (str_detect(instr, "mask")) {
    mask <- str_match(instr, "mask = (.+)")[1, 2] %>% 
      str_c(str_c(rep("X", 28), collapse = ""), .)
  } else {
    address <- str_match(instr, "mem\\[(\\d+)\\]")[1, 2] %>% as.integer()
    number <- str_match(instr, "= (\\d+)")[1, 2] %>% as.integer64()
    bits <- as.bitstring(number)
    masked_bits <- map_chr(seq_len(nchar(bits)), function(i) {
      if (str_sub(mask, i, i) == "X") str_sub(bits, i, i)
      else str_sub(mask, i, i)
    }) %>% paste(collapse = "")
    memory[address] <- as.integer64.bitstring(masked_bits)
  }
}
sum(memory)


# part 2 ====
memory <- list()
for (i in seq_along(input)) {
  instr <- input[i]
  if (str_detect(instr, "mask")) {
    mask <- str_match(instr, "mask = (.+)")[1, 2] %>% 
      str_c(str_c(rep("0", 28), collapse = ""), .)
  } else {
    address <- str_match(instr, "mem\\[(\\d+)\\]")[1, 2] %>% as.integer64()
    number <- str_match(instr, "= (\\d+)")[1, 2] %>% as.integer64()
    bits <- as.bitstring(address)
    masked_bits <- map(seq_len(nchar(bits)), function(x) {
      if (str_sub(mask, x, x) == "0") {
        str_sub(bits, x, x)
      } else if (str_sub(mask, x, x) == "1") {
        c("1")
      } else {
        c("0", "1")
      }
    })
    bit_options <- expand.grid(masked_bits, stringsAsFactors = FALSE)
    for (row in seq_len(nrow(bit_options))) {
      masked_address <- paste(bit_options[row, ], collapse = "") %>% 
        as.integer64.bitstring()
      memory[[as.character(masked_address)]] <- number
    }
  }
}
reduce(memory, sum)
