library(tidyverse)
library(stringr)
is_valid <- function(phrase) {
  word_list <- str_split(phrase, fixed(" ")) %>% flatten()
  !any(map_lgl(seq_along(word_list), ~ any(word_list[[.x]] %in% word_list[-.x])))
}

read_lines("inputs/q4.txt") %>%
  map_lgl(is_valid) %>%
  sum()
