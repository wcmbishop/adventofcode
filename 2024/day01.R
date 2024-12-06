library(purrr)
library(readr)
library(stringr)

# puzzle 1 =====

input1 <- read_lines("data/day01-input1.txt")
lists <- str_split(input1, "\ +")
left <- map_int(lists, ~as.integer(.x[1]))
right <- map_int(lists, ~as.integer(.x[2]))
distances <- abs(sort(left) - sort(right))
sum(distances)


# puzzle 2 =====

occurrence <- map_int(left, ~sum(.x == right))
similarity <- left*occurrence
sum(similarity)

