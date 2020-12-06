library(dplyr)
library(purrr)
library(stringr)
library(testthat)
library(tibble)
library(tidyr)

# read input file
passports_raw <- file.path("data-raw", "day-04", "passports.txt") %>% 
  readLines()


# part 1 ====
# parse lines for a single passport
parse_passport_fields <- function(lines) {
  kv_pairs <- str_split(lines, pattern = " ") %>% 
    unlist() %>% 
    str_split(pattern = ":")
  passport <- list()
  for (kv_pair in kv_pairs) {
    passport[[kv_pair[1]]] = kv_pair[2]
  }
  as_tibble(passport)
}

# find text lines for each passport
all_lines <- seq_along(passports_raw)
blank_lines <- which(passports_raw == "")
text_lines <- which(!(all_lines %in% blank_lines))
# create dataframe of passports, each with the source line numbers 
passports <- tibble(line = text_lines) %>% 
  mutate(
    line_delta = line - lag(line),
    line_delta = if_else(is.na(line_delta), 1L, line_delta),
    index = 1 + cumsum(line_delta - 1)
  ) %>% 
  group_by(index) %>% 
  summarise(line_numbers = list(unique(line)))
# parse source text
passports <- passports %>% 
  # add text lines for each passport
  mutate(text_lines = map(line_numbers, ~passports_raw[.])) %>% 
  # parse fields for each passport
  mutate(fields = map(text_lines, ~parse_passport_fields(.)))

# check for required fields
required_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
passports <- passports %>% 
  mutate(is_valid = map_lgl(fields, ~all(required_fields %in% names(.))))
passports %>% 
  count(is_valid)

# part 2 ====
passport_is_valid <- function(data) {
  # required fields
  required_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  if (!all(required_fields %in% names(data)))
    return(FALSE)
  # byr (Birth Year) - four digits; at least 1920 and at most 2002.
  if (!isTRUE(between(as.integer(data$byr), 1920, 2002)))
    return(FALSE)
  # iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  if (!isTRUE(between(as.integer(data$iyr), 2010, 2020)))
    return(FALSE)
  # eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  if (!isTRUE(between(as.integer(data$eyr), 2020, 2030)))
    return(FALSE)
  # hgt (Height) - a number followed by either cm or in:
  #  If cm, the number must be at least 150 and at most 193.
  #  If in, the number must be at least 59 and at most 76.
  hgt_value <- as.numeric(str_match(data$hgt, "([0-9]+).+")[1, 2])
  hgt_units <- str_match(data$hgt, "[0-9]+(.+)")[1, 2]
  if (hgt_units == "cm") {
    if (!isTRUE(between(hgt_value, 150, 193)))
      return(FALSE)
  } else if (hgt_units == "in") {
    if (!isTRUE(between(hgt_value, 59, 76)))
      return(FALSE)
  } else {
    return(FALSE)
  }
  # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  if (!str_detect(data$hcl, pattern = "^#[0-9a-f]{6}$"))
    return(FALSE)
  # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  eye_colors <- c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  if (!isTRUE(data$ecl %in% eye_colors))
    return(FALSE)
  # pid (Passport ID) - a nine-digit number, including leading zeroes.
  if (!str_detect(data$pid, pattern = "^[0-9]{9}$"))
    return(FALSE)
  # cid (Country ID) - ignored, missing or not.
  TRUE
}

# re-validate 
passports <- passports %>% 
  mutate(is_valid2 = map_lgl(fields, ~passport_is_valid(.)))
passports %>% 
  count(is_valid2)
