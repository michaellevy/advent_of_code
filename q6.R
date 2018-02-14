library(tidyverse)
library(stringr)
init <- "2	8	8	5	4	2	3	1	5	5	1	2	15	13	5	14" %>%
  str_split("\\s") %>% .[[1]] %>% as.integer()

go_init <- function(state, len) {
  i <- 1L
  states <- character(len)
  while (TRUE) {
    state <- redist(state)
    if (i %% 100 == 0) print(i)
    char_state <- paste(state, collapse = "")
    if (char_state %in% states) return(i) else states[[i]] <- char_state
    i <- i + 1L
  }
}

redist <- function(state, i = which.max(state), n = length(state)) {
  to_dist <- state[i]
  state[i] <- 0L
  state <- state + floor(to_dist / n)
  remain <- to_dist %% n
  to_add <- if (i == n) {
    seq_len(remain)
  } else if (remain + i <= n) {
    i + (seq_len(remain))
  } else {
    c((i + 1):n, seq_len(remain - (n - i)))
  }
  state[to_add] <- state[to_add] + 1L
  return(state)
}

go_init(init, 1e4)
