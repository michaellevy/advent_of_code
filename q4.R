library(tidyverse)
library(stringr)
is_valid <- function(phrase) {
  word_list <- str_split(phrase, fixed(" ")) %>% flatten()
  length(word_list) == length(unique(word_list))
}

read_lines("inputs/q4.txt") %>%
  map_lgl(is_valid) %>%
  sum()

is_valid2 <- function(phrase) {
  word_list <-
    str_split(phrase, fixed(" ")) %>%
    flatten() %>%
    map_chr(~ {
      str_split(.x, "")[[1]] %>%
        sort() %>%
        paste(collapse = "")
      })
  length(word_list) == length(unique(word_list))
}

read_lines("inputs/q4.txt") %>%
  map_lgl(is_valid2) %>%
  sum()
