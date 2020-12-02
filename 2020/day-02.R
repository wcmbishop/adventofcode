library(dplyr)
library(stringr)
library(tibble)
library(tidyr)

passwords_raw <- readLines(file.path("data-raw", "day-02", "passwords.txt"))


# part 1 ====
password <- tibble(raw_input = passwords_raw) %>% 
  separate(raw_input, c("policy", "password"), sep = ": ", remove = FALSE) %>% 
  mutate(
    policy_min = as.integer(str_match(policy, "([\\d]+)-[\\d]+ .*")[, 2]),
    policy_max = as.integer(str_match(policy, "[\\d]+-([\\d]+) .*")[, 2]),
    policy_char = str_match(policy, "[\\d]+-[\\d]+ (.+)")[, 2],
    char_count = str_count(password, policy_char),
    is_valid = char_count >= policy_min & char_count <= policy_max
  )
password %>%
  count(is_valid)


# part 2 ====
# apply different password validation
password <- password %>% 
  mutate(
    password_char_a = str_sub(password, policy_min, policy_min),
    password_char_b = str_sub(password, policy_max, policy_max),
    is_valid2 = xor(password_char_a == policy_char, 
                    password_char_b == policy_char)
  )
password %>%
  count(is_valid2)
