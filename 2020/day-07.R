library(dplyr)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
input_raw <- file.path("data-raw", "day-07", "luggage-rules.txt") %>% 
  readLines()
BAG_REGEX <- "[a-z]+ [a-z]+ bag"

# part 1 ====
parse_child_bags <- function(text) {
  # parse child bags text into dataframe of child bags (count, name)
  bags_text <- str_extract_all(text, paste("[\\d]+", BAG_REGEX))[[1]]
  map_dfr(bags_text, function(x) {
    list(child_count = str_extract(x, "[\\d]") %>% as.integer(),
         child_bag = str_extract(x, BAG_REGEX))
  })
}
# parse bag rules - each row is a bag and its child-bags (a nested data-frame)
bag_rules <- tibble(rule_text = input_raw)
bag_rules <- bag_rules %>% 
  separate(rule_text, into = c("bag_text", "child_bag_text"), 
           sep = "contain", remove = FALSE) %>% 
  mutate(bag = str_extract(bag_text, BAG_REGEX)) %>% 
  mutate(child_bags = map(child_bag_text, parse_child_bags)) %>% 
  select(bag, child_bags, rule_text)

# expand to all the parent/child relationships - each row is a bag and a child bag
bag_links <- bag_rules %>% 
  select(bag, child_bags) %>% 
  unnest(child_bags)

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
parent_bags <- find_all_parent_bags(bag_links, bags = "shiny gold bag")
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

child_bags <- find_all_child_bags(bag_links, bags = "shiny gold bag", 
                                  bag_counts = 1)
sum(child_bags$total_child_count)

