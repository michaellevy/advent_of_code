library(tidyverse)

# For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.

check <- function(x) {
  apply(x, MARGIN = 1, function(r) max(r) - min(r)) %>%
    sum()
}
read_tsv("inputs/q2.tsv", col_names = FALSE) %>%
  check()

### Part 2

# find the only two numbers in each row where one evenly divides the other - that is, where the result of the division operation is a whole number. They would like you to find those numbers on each line, divide them, and add up each line's result.
#
# For example, given the following spreadsheet:
#
# 5 9 2 8
# 9 4 7 3
# 3 8 6 5
# In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
# In the second row, the two numbers are 9 and 3; the result is 3.
# In the third row, the result is 2.

check_div <- function(x) {
  apply(x, MARGIN = 1, function(r) {
    combos <- expand.grid(r, r)
    # Get rid of the self-pairings
    combos <- combos[-seq(1L, nrow(combos), length.out = length(r)), ]
    winner <-
      combos %>%
      mutate(div = Var1 / Var2) %>%
      filter(div %% 1 == 0) %>%
      pull(div)
    if (length(winner) == 1) {
      return(winner)
    } else {
      stop("Expected one evenly divisble pair; found ", length(winner))
    }
  }) %>%
    sum()
}
read_tsv("inputs/q2.tsv", col_names = FALSE) %>%
  check()
