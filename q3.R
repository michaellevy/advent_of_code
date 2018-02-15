steps_to_center <- function(input) {
  bottomright <- 1L
  i <- 1L
  while (bottomright < input) {
    bottomright <- bottomright + 4L * i * 2
    i <- i + 1L
  }
  grid_size <- sqrt(bottomright)
  count_behind <- bottomright - input
  corner_offset <- count_behind %% (grid_size - 1)
  short_dist_center <- abs(corner_offset - (grid_size - 1) / 2)
  return(short_dist_center + (grid_size - 1) / 2)
}

steps_to_center(312051)

# Part 2

## Try this manually, with n as the center point of the grid
get_dir <- function(m = m, pos = pos, dirs = dirs, dir = dir) {
  if (dir == "r") {
    if (is.na(m[pos + dirs$u])) "u" else "r"
  } else if (dir == "u") {
    if (is.na(m[pos + dirs$l])) "l" else "u"
  } else if (dir == "l") {
    if (is.na(m[pos + dirs$d])) "d" else "l"
  } else {
    if (is.na(m[pos + dirs$r])) "r" else "d"
  }
}

n = 11
m <- matrix(NA_integer_, nrow = n * 2 - 1, ncol = n * 2 - 1)
pos <- cbind(n, n)
m[pos] <- 1L
dirs <- list(d = cbind(1, 0), u = cbind(-1, 0), l = cbind(0, -1), r = cbind(0, 1))
dir <- "r"

prev <- 1L
while(prev < 312051) {
  if (is.na(m[pos])) {
    m[pos] <- sum(m[as.matrix(expand.grid(x = pos[, 1] + -1:1, y = pos[, 2] + -1:1))], na.rm = TRUE)
    dir <- get_dir(m, pos, dirs, dir)
  }
  prev <- m[pos]
  pos <- pos + dirs[[dir]]
}
m
prev
