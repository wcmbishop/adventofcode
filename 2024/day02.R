library(purrr)
library(readr)
library(stringr)


# part 1 =====

input1 <- read_lines("data/day02-input1.txt")
reports <- str_split(input1, " ") %>%
  map(~as.integer(.x))

is_report_safe <- function(levels) {
  level_deltas <- diff(levels)
  min_delta <- min(abs(level_deltas))
  max_delta <- max(abs(level_deltas))
  is_sign_match <- length(unique(sign(level_deltas))) == 1
  is_sign_match && min_delta >= 1 && max_delta <= 3
}

map_lgl(reports, ~is_report_safe(.x)) %>% 
  sum()


# part 2 =====

is_report_safe2 <- function(levels) {
  is_safe <- is_report_safe(levels)
  if (is_safe == FALSE) {
    for (i in seq_along(levels)) {
      is_safe <- is_report_safe(levels[-i])
      if (is_safe) 
        break
    }
  }
  is_safe
}

map_lgl(reports, ~is_report_safe2(.x)) %>% 
  sum()

