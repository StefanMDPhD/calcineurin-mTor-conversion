likelihood.limits <- function(alpha = 0.05, beta = 0.05)
{
  r.l <- alpha/(1 - beta)
  invisible(list(alpha = alpha, beta = beta, r.u = 1/r.l, r.l = r.l))
}