library(glue)
library(purrr)

input_file <- file.path("data-raw", "day-01", "expense-report.txt")
expenses <- readLines(input_file) %>% as.numeric()


# part 1 ====
walk(
  expenses,
  ~ walk(
    expenses,
    a = .x,
    function(a = a, b = .x) {
      if (a + b == 2020) {
        message(glue("{a} + {b} = 2020! a * b = {y}", y = a*b))
      }
    }
  )
)

# part 2 ====
# unique combinations for groups of 3
combinations <- combn(expenses, 3, simplify = FALSE)
i <- which(map_dbl(combinations, sum) == 2020)
combinations[[i]]
prod(combinations[[i]])

