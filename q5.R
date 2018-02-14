library(tidyverse)
step_it <- function(steps) {
  steps_taken <- 0L
  current <- 1L
  length_steps <- length(steps)
  while (TRUE) {
    next_up <- current + steps[current]
    steps[current] <- steps[current] + 1L
    steps_taken <- steps_taken + 1L
    if (next_up > length_steps || next_up < 1) {
      break
    } else {
      current <- next_up
    }
  }
  return(steps_taken)
}
read_lines("inputs/q5.txt") %>%
  as.integer() %>%
  step_it()

step_it2 <- function(steps) {
  steps_taken <- 0L
  current <- 1L
  length_steps <- length(steps)
  while (TRUE) {
    next_up <- current + steps[current]
    steps[current] <- steps[current] +
      if (steps[current] >= 3) -1L else 1L
    steps_taken <- steps_taken + 1L
    if (next_up > length_steps || next_up < 1) {
      break
    } else {
      current <- next_up
    }
  }
  return(steps_taken)
}
read_lines("inputs/q5.txt") %>%
  as.integer() %>%
  step_it2()
