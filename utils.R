#' @param x Numeric vector of the time series of the signal.
#' @param min.lag Integer, only lags > min.lag will be considered.
#' @param lag.max Integer, maximum lag to consider.
#' @param demean Boolean.
#' @param plot Boolean.
best_lag <- function(
  x,
  min.lag,
  lag.max,
  demean,
  plot
) {
  args <- match.call(expand.dots = TRUE)
  ac <- acf(
    x,
    lag.max = lag.max,
    type = "correlation",
    demean = demean,
    plot = plot
  )
  res <- data.frame(
    lag = as.vector(ac$lag),
    autocorrelation = as.vector(ac$acf)
  )
  if (plot) plot(res, type = "b")
  res <- res[with(res, lag > min.lag), ]
  res <- res[with(res, order(autocorrelation, decreasing = TRUE)), ]
  res <- res[1, ]
  if (plot) points(res, col = "tomato", pch = 3, cex = 3, lw = 3)
  return(res)
}

#' @param x Numeric vector of the time series of the signal.
bumps <- function(x) {
  mins <- which(diff(sign(diff(x))) == -2) + 1
  max <- which(diff(sign(diff(x))) == 2) + 1
  res <- data.frame(signal = x, step = seq_along(x))
  res$bump <- NA
  res$bump[mins] <- "minimum"
  res$bump[max] <- "maximum"
  res <- res[with(res, !is.na(bump)), ]
  return (res)
}

#' @param x Numeric vector of the time series of the signal.
#' @param min.lag Integer, only lags > min.lag will be considered.
#' @param lag.max Integer, maximum lag to consider.
#' @param demean Boolean.
#' @param plot Boolean.
periodicity_signal <- function(
  x,
  min.lag,
  lag.max,
  demean,
  plot = FALSE
) {
  oldpar <- par(no.readonly = TRUE)
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
  if (plot) plot(seq_along(x), x, type = "l", xlab = "step", ylab = "signal")
  # find best lag -----------
  lag <- best_lag(
    x,
    min.lag,
    lag.max,
    demean,
    plot = plot
  )
  # get extremes ----------
  b <- bumps(x)
  if (plot) {
    step <- seq_along(x)
    plot(
      step[1 : (lag$lag * 2)],
      signal[1 : (lag$lag * 2)],
      type = "l",
      xlab = "step", ylab = "signal"
    )
    abline(v = seq(1, 1e3, by = lag$lag), col = "tomato", lw = 4)
    abline(v = step[b$step[b$bump == "minimum"]], col = "dodgerblue3", lw = 2)
    abline(v = step[b$step[b$bump == "maximum"]], col = "gold2", lw = 2)
  }

  # get period -------------
  b <- b[with(b, step < lag$lag), ]
  period <- nrow(b)

  par (oldpar)
  return (list(
    best.lag = lag,
    period = period,
    extremes = b
  ))
}
