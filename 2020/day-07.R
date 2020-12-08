library(dplyr)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
input_raw <- file.path("data-raw", "day-07", "luggage-rules.txt") %>% 
  readLines()

# part 1 ====
# extract all links between bags and their child bags
bag_links <- tibble(raw_text = input_raw) %>% 
  extract(raw_text, into = c("bag", "contents"), 
          regex = "(.*) bags contain (.*)\\.") %>% 
  separate_rows(contents, sep = ", ") %>% 
  extract(contents, into = c("child_count", "child_bag"),
          regex = "([0-9]*) ([a-z]+ [a-z]+) bag", 
          convert = TRUE, remove = FALSE) %>% 
  filter(!is.na(child_count))

# find all possible parent bags of a "shiny gold bag"
find_all_parent_bags <- function(bag_links, bags) {
  parent_bags <- unique(bag_links$bag[bag_links$child_bag %in% bags])
  if (length(parent_bags) == 0) {
    return(NULL)
  } else {
    parent_bags <- c(parent_bags, find_all_parent_bags(bag_links, parent_bags))
  }
  unique(parent_bags)
}
parent_bags <- find_all_parent_bags(bag_links, bags = "shiny gold")
length(parent_bags)


# part 2 ====
# find all possible child bags of a given parent bag
find_all_child_bags <- function(bag_links, bags, bag_counts) {
  bag_df <- tibble(bag = bags, bag_count = bag_counts)
  child_bags <- bag_links %>% 
    filter(bag %in% bags) %>% 
    inner_join(bag_df, by = "bag") %>% 
    mutate(total_child_count = bag_count * child_count)
  if (nrow(child_bags) == 0) {
    return(NULL)
  } else {
    child_bags <- bind_rows(
      child_bags, 
      find_all_child_bags(bag_links, bags = child_bags$child_bag,
                          bag_counts = child_bags$total_child_count)
    )
  }
  child_bags
}

child_bags <- find_all_child_bags(
  bag_links, bags = "shiny gold", bag_counts = 1)
sum(child_bags$total_child_count)
