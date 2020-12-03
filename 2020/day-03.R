library(dplyr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)


# build tree map
map_raw <- readLines(file.path("data-raw", "day-03", "tree-map.txt"))
nrows <- length(map_raw)
ncols <- nchar(map_raw[[1]])
tree_vec <- str_split(map_raw, pattern = "") %>% unlist() == "#"
tree_map <- matrix(tree_vec, ncol = ncols, byrow = TRUE)
expect_equal(nrows, nrow(tree_map))
expect_equal(ncols, ncol(tree_map))


# part 1 ====
build_path <- function(dim, start, move) {
  rows <- seq(start[1], dim[1], by = move[1])
  cols <- seq(start[2], length.out = length(rows), by = move[2])
  # have columns wrap around within dim bounds
  cols <- (cols - 1) %% dim[2] + 1
  list(rows = rows, cols = cols)
}
count_path_trees <- function(path, tree_map) {
  path_trees <- map2_lgl(path$rows, path$cols, ~tree_map[.x, .y])
  sum(path_trees)
}
# sledd path: right 3, down 1
# (note: inputs represent row/col pairs)
path <- build_path(dim = c(nrows, ncols), start = c(1, 1), move = c(1, 3))

# find the tree values along the path
count_path_trees(path, tree_map)

# part 2 ====
# build a tibble of paths to calculate the tree count for each
paths <- tribble(
  ~path_name, ~right, ~down,
  "A", 1, 1,
  "B", 3, 1,
  "C", 5, 1,
  "D", 7, 1,
  "E", 1, 2
) 
paths <- paths %>% 
  rowwise() %>%
  mutate(
    # define path object for each row
    path = list(build_path(
      dim = c(nrows, ncols), 
      start = c(1, 1), move = c(down, right)
    )),
    # calculate path tree count for each row
    tree_count = count_path_trees(path, tree_map)
  )
# multiply tree count across all paths
prod(paths$tree_count)
