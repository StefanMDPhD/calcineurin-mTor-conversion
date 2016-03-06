estimate.ar1 <- function(series)
{
  #
  # Length of series
  #
  n <- length(series)
  #
  # Centre and truncate series
  #
  centred.series <- series - mean(series[-n])
  truncated.series <- centred.series[-n]
  #
  # Check computation of variance
  #
  sigma2.hat <- truncated.series %*% truncated.series / (n - 2)
  s2.hat <- var(series[-n])
  #
  # Re-centre and truncate series (acf uses mean of all time points to centre)
  #
  centred.series <- series - mean(series)
  truncated.series <- centred.series[-n]
  #
  # Check computation of autocorrelation
  #
  autocovariance.hat <- centred.series[-1] %*% centred.series[-n] / n
  a2.hat <- acf(series, lag.max = 1, type = "covariance", na.action = na.fail)$acf
  #
  series.mean <- mean(series)
  centred.series <- series - series.mean
#  acf1 <- (series[-n] - series.mean) %*% (series[-1] - series.mean) /
#    (series - series.mean) %*% (series - series.mean)
  ar1.hat <- centred.series[-n] %*% centred.series[-1] /
    centred.series[-length(centred.series)] %*% centred.series[-length(centred.series)]
  #
  # Estimate the AR(1) model
  #
  series.ar1 <- ar(series, aic = FALSE, order.max = 1, method = "ols", na.action = na.fail,
                   var.method = "", dmean = TRUE, intercept = FALSE)
  s.mean <- series.ar1$x.mean
  s.ar1 <- series.ar1$ar
  s.sigma1 <- series.ar1$var.pred
  #
  errors <- centred.series[-1] - ar1.hat * centred.series[-n]
  sigma1.hat <- errors %*% errors / (n - 1)
  # NOT any of the following (since we assume mean = 0)
  #   (1) var(errors)
  #   (2) errors %*% errors / (length(errors) - 1)
  #
  # Output
  #
  invisible(list(series.mean = series.mean, s.mean = s.mean,
                 ar1.hat = ar1.hat, s.ar1 = s.ar1,
                 sigma1.hat = sigma1.hat, s.sigma1 = s.sigma1,
                 series.ar1 = series.ar1))
}