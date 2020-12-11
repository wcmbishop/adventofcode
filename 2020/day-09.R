library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
all_numbers <- file.path("data-raw", "day-09", "numbers.txt") %>% 
  readLines() %>% 
  as.numeric()
preamble_length <- 25

# part 1 ====
has_addition_matches <- function(total, numbers) {
  total %in% combn(numbers, m = 2, sum)
}
# search for first number without matching sum
index <- preamble_length + 1
while (TRUE) {
  if (index > length(all_numbers)) {
    print("finished")
    break
  }
  match <- has_addition_matches(
    all_numbers[index], 
    all_numbers[(index - preamble_length):(index - 1)]
  )
  if (match == FALSE) {
    print(glue("found a non-motch: index {index}, number {number}",
               index = index, number = all_numbers[index]))
    break
  }
  index <- index + 1
}


# part 2 ====
invalid_num <- 556543474
indices <- seq_along(all_numbers)
for (chunk_start in indices) {
  chunk_found <- FALSE
  for (chunk_end in indices[indices > chunk_start]) {
    chunk <- all_numbers[chunk_start:chunk_end]
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
