phrases <- list("i am not a robot.", "i do have a repeat i do", "no not here")

library(tidyverse)
library(stringr)
is_valid <- function(phrase) {
  word_list <- str_split(phrase, fixed(" ")) %>% flatten()
  !any(map_lgl(seq_along(word_list), ~ any(word_list[[.x]] %in% word_list[-.x])))
}
map_lgl(phrases, is_valid)
