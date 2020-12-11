library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
numbers <- file.path("data-raw", "day-09", "numbers.txt") %>% 
  readLines() %>% 
  as.numeric()
preamble_length <- 25

# part 1 ====
has_addition_matches <- function(total, numbers) {
  total %in% combn(numbers, m = 2, sum)
}
# search for first number without matching sum
for (i in seq_along(numbers)) {
  if (i <= preamble_length) next
  has_match <- has_addition_matches(
    total = numbers[i], numbers = numbers[(i - preamble_length):(i - 1)]
  )
  if (has_match == FALSE) {
    print(glue(
      "found a non-motch: index {i}, number {number}",
      i = i, number = numbers[i]
    ))
    break
  }
}


# part 2 ====
invalid_num <- 556543474
indices <- seq_along(numbers)
for (chunk_start in indices) {
  chunk_found <- FALSE
  for (chunk_end in indices[indices > chunk_start]) {
    chunk <- numbers[chunk_start:chunk_end]
    if (sum(chunk) == invalid_num) {
      print("chunk found!")
      print(chunk)
      print(sum(min(chunk), max(chunk)))
      chunk_found <- TRUE
      break
    }
  }
  if (chunk_found) break
}
