# DAY 8
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)

# read input data
input <- readLines(file.path("data-raw", "day8.txt"), warn = FALSE)


# part 1 ====
width <- 25
height <- 6
n_pixels <- width * height

input_pixels <- str_split(input, "")[[1]]
n_layers <- ceiling(length(input_pixels) / n_pixels)

pixels <- tidyr::crossing(
  layer = seq_len(n_layers),
  row = seq_len(height),
  col = seq_len(width)
) %>% 
  mutate(pixel = as.integer(input_pixels))

layer <- pixels %>% 
  group_by(layer) %>% 
  summarise(n_zeros = sum(pixel == 0),
            n_ones = sum(pixel == 1),
            n_twos = sum(pixel == 2)) %>% 
  filter(n_zeros == min(n_zeros))
layer$n_ones * layer$n_twos


# part 2 ====
# find the first non-zero pixel layer for each position
image <- pixels %>% 
  mutate(pixel = if_else(pixel == 2, NA_integer_, pixel)) %>% 
  group_by(row, col) %>% 
  summarise(pixels = list(pixel),
            pixel = purrr::map_int(pixels, ~na.omit(.x)[1])) %>% 
  ungroup()
# plot it
ggplot(image, aes(col, row)) +
  geom_tile(aes(fill = as.factor(pixel))) +
  scale_y_reverse()
