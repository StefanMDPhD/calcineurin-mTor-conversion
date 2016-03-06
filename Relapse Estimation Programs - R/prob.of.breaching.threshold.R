prob.of.breaching.threshold <- function(series, threshold)
{
  #
  # Length of series
  #
  n <- length(series)
  #
  # Re-centre and truncate series (acf uses mean of all time points to centre)
  #
  centred.series <- series - mean(series)
  truncated.series <- centred.series[-n]
  #
  # Check computation of autocorrelation
  #
  autocovariance.hat <- centred.series[-1] %*% centred.series[-n] / n
  #
  ar1.hat <- centred.series[-n] %*% centred.series[-1] /
    centred.series[-length(centred.series)] %*% centred.series[-length(centred.series)]
  #
  errors <- centred.series[-1] - ar1.hat * centred.series[-n]
  sigma1.hat <- errors %*% errors / (n - 1)
  # NOT any of the following (since we assume mean = 0)
  #   (1) var(errors)
  #   (2) errors %*% errors / (length(errors) - 1)
  #
  lambda <- mean(series) * (1 - ar1.hat)
  mt <- ar1.hat * series[n] + lambda
  prob <- pnorm((threshold - mt)/sigma1.hat)
  #
  # Output
  #
  invisible(list(prob = prob,
                 ar1.hat = ar1.hat, sigma1.hat = sigma1.hat))
}