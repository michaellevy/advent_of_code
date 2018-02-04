steps <- c(0, 3, 0, 1, -3)
current <- 1
while (TRUE) {
  length_steps <- length(steps)
  next_up <- current + steps[current]
  steps[current] <- steps[current] + 1
  if (next_up > length_steps || next_up < 1) {
    print(steps)
    break
  } else {
    current <- next_up
  }
}
