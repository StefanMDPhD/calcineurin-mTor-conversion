design.attempt <- function(n1, a1, n2 = 0, a = 0, p0 = 0.7, p1 = 0.85)
{
  #
  # If one stage design (n2 = 0), then force a = a1
  #
  if(n2 == 0) {
    a <- a1
  }
  alpha = 0
  power = 0
  for(k1 in a1:n1) {
    for(y1 in max(0,-k1):(n1-max(0,k1))) {
      for(k2 in (a-k1):n2) {
        for(y2 in max(0,-k2):(n2-max(0,k2))) {
          alpha <- alpha + dbinom(y1, n1, p0) * dbinom(k1+y1, n1, p0) *
            dbinom(y2, n2, p0) * dbinom(k2 + y2, n2, p0)
          power <- power + dbinom(y1, n1, p0) * dbinom(k1+y1, n1, p1) *
            dbinom(y2, n2, p0) * dbinom(k2 + y2, n2, p1)
        }
      }
    }
  }
  #
  # Probability of early termination
  #
  PET <- 0
  for(k1 in (-n1):(a1-1)) {
    for(y1 in max(0, -k1):(n1 - max(0, k1))) {
      PET <- PET + dbinom(y1, n1, p0) * dbinom(k1 + y1, n1, p0)
    }
  }
  EN <- n1 * PET + (n1 + n2) * (1 - PET)
  invisible(list(alpha = alpha, power = power, PET = PET, EN = EN))
}