estimate.lr <- function(times, series)
{
  #
  # Compute means
  #
  mean.times <- mean(times)
  mean.series <- mean(series)
  #
  # Sweep means
  #
  centred.times <- times - mean.times
  centred.series <- series - mean.series
  #
  # Compute coefficients
  #
  b1 <- centred.times %*% centred.series / centred.times %*% centred.times
  b0 <- mean.series - b1 * mean.times
  #
  # R linear regression
  #
  data <- data.frame(series, times)
  attach(data)
  lm.out <- lm(series ~ times)
  detach(data)
  #
  # Predict next value in series
  #
  pred.series <- b0 + b1*(times[length(times)]+mean(diff(times)))
  #
  # Output
  #
  invisible(list(mean.times = mean.times, mean.series = mean.series,
                 b0 = b0, b1 = b1, lm.out = lm.out,
                 pred.series = pred.series))
}