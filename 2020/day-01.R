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
walk(
  combinations,
  function(x) {
    if (sum(x) == 2020) {
      message(glue("{a} + {b} + {c} = 2020! a*b*c = {y}", 
                   a = x[1], b = x[2], c = x[3], y = prod(x)))
    }
  }
)
