source("utils.R")

# first signal --------------
time <- seq_len(1e3)
A <- sin(.2 * time)
B <- 3 * sin(.1 * time)
C <- 1.2 * sin(.5 * time)
signal <- A + B + C

periodicity_signal(
  signal,
  min.lag = 10,
  lag.max = 1e2,
  demean = FALSE,
  plot = TRUE
)

# second signal ------------------
A <- 0.3 * sin(.52 * time)
B <- 2 * sin(.05 * time)
C <- sin(.05 * time)
signal2 <- A + B + C

periodicity_signal(
  signal2,
  min.lag = 1e2,
  lag.max = 1e3,
  demean = FALSE,
  plot = TRUE
)
