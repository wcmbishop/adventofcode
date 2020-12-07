library(dplyr)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
input_raw <- file.path("data-raw", "day-06", "form-answers.txt") %>% 
  readLines()


# part 1 ====
# find text lines for each group
all_lines <- seq_along(input_raw)
blank_lines <- which(input_raw == "")
text_lines <- which(!(all_lines %in% blank_lines))
# create dataframe of lines for each group
answers <- tibble(line = text_lines) %>% 
  mutate(
    line_delta = line - lag(line),
    line_delta = if_else(is.na(line_delta), 1L, line_delta),
    group = 1 + cumsum(line_delta - 1)
  ) %>% 
  group_by(group) %>% 
  summarise(line_numbers = list(unique(line))) 
# load and parse text lines for each group to count answers
answers <- answers %>%  
  mutate(
    text_lines = map(line_numbers, ~input_raw[.]),
    unique_answers = map(text_lines, function(x) {
      str_split(x, "") %>% unlist() %>% unique()
    }),
    answer_count = map_dbl(unique_answers, ~length(.))
  ) 

sum(answers$answer_count)


# part 2 ====
# reprocess answer counts to find shared answers within each group
answers <- answers %>% 
  mutate(
    shared_answers = map(text_lines, ~Reduce(intersect, str_split(., ""))),
    shared_answer_count = map_dbl(shared_answers, ~length(.))
  )
sum(answers$shared_answer_count)
