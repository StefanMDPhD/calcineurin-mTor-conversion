non.inferior <- function(alpha, beta, pi, d)
{
  invisible((qnorm(1 - alpha) + qnorm(1 - beta))^2 * 2 * pi * (1 - pi)/d^2)
}