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
