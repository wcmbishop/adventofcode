library(dplyr)
library(readr)
library(stringr)

# part 1 =====
input <- read_lines("data/day04-input1.txt")
n <- length(input)

grid <- str_split(input, pattern = "") %>% 
  unlist() %>% 
  matrix(nrow = n, byrow = TRUE)
# rotate <- function(x) t(apply(x, 2, rev))
# rotate(grid)

# build horizontal and vertical strings
input_hor <- rep(NA_character_, n)
input_vert <- rep(NA_character_, n)
for (i in seq_len(n)) {
  input_hor[i] <- str_c(grid[i, ], collapse = "")
  input_vert[i] <- str_c(grid[, i], collapse = "")
}

# build diagonal strings
n_diag <- 2*n - 1
input_diag1 <- rep(NA_character_, n)
input_diag2 <- rep(NA_character_, n)
for (i in seq_len(n_diag)) {
  diag1 <- ""
  diag2 <- ""
  for (j in seq_len(n)) {
    # diagonal 1
    c1 <- j
    r1 <- i + 1 - c1
    if (between(r1, 1, n) && between(c1, 1, n))
      diag1 <- str_c(diag1, grid[r1, c1])
    # diagonal 2
    c2 <- j
    r2 <- n - i + 1 + c2 - 1
    if (between(r2, 1, n) && between(c2, 1, n))
      diag2 <- str_c(diag2, grid[r2, c2])
  }
  input_diag1[i] <- diag1
  input_diag2[i] <- diag2
}

matches <- purrr::map(
  list(input_hor, input_vert, input_diag1, input_diag2),
  ~c(
    str_count(.x, "XMAS"), str_count(.x, "SAMX")
  )
) %>% unlist()
sum(matches)


# part 2 =====




