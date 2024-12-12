library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)


# part 1 =====

input1 <- read_lines("data/day03-input1.txt")
pattern <- "mul\\((?<first>[0-9]+),(?<second>[0-9]+)\\)"
matches_list <- str_match_all(input1, pattern)

matches <- purrr::map_dfr(
  matches_list,
  .id = "row",
  ~as_tibble(.x) %>% 
    select(instr = V1, first, second) %>% 
    mutate(across(c("first", "second"), as.integer))
)
matches %>% 
  mutate(product = first*second) %>% 
  summarise(total = sum(product))


# part 2 =====
parse_matches <- function(memory, is_enabled = TRUE) {
  pattern <- "mul\\((?<first>[0-9]+),(?<second>[0-9]+)\\)"
  matches_list <- str_c(memory, collapse = "") %>% 
    str_match_all(pattern)
  matches <- as_tibble(matches_list[[1]]) %>% 
      select(instr = V1, first, second) %>% 
      mutate(across(c("first", "second"), as.integer)) %>% 
      mutate(product = first*second)
  if (!is_enabled)
    matches <- matches[0,]
  matches
}

parse_section <- function(memory, is_enabled = TRUE) {
  next_pattern <- ifelse(is_enabled, "don\\'t\\(\\)", "do\\(\\)")
  next_loc <- str_locate(memory, next_pattern)
  
  if (is.na(next_loc[1, "start"])) {
    current_matches <- parse_matches(memory, is_enabled)
    next_matches <- NULL  
  } else {
    current_matches <- parse_matches(
      str_sub(memory, start = 1, end = next_loc[1, "end"] - 1),
      is_enabled
    )
    next_matches <- parse_section(
      memory = str_sub(memory, start = next_loc[1, "end"] + 1, end = -1),
      is_enabled = ifelse(is_enabled, FALSE, TRUE)
    )
  }
  bind_rows(current_matches, next_matches)
}

matches <- parse_section(input1)
matches %>% 
  summarise(total = sum(product))

