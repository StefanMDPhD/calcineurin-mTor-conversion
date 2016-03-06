single.stage.design.for.n1 <- function(n1, a1.range = 5:10, p0 = 0.7, p1 = 0.85, alpha = 0.05, power = 0.8)
{
  designs <- array(NA, dim = c(length(a1.range),6),
                   dimnames = list(as.character(a1.range), c("n", "a", "PET", "EN", "alpha", "power")))
  designs[, "n"] <- n1
  for(a1 in a1.range) {
    temp <- design.attempt(n1 = n1, a1 = a1, n2 = 0, a = a1, p0 = p0, p1 = p1)
    designs[as.character(a1), "a"] <- a1
    designs[as.character(a1), "PET"] <- temp$PET
    designs[as.character(a1), "EN"] <- temp$EN
    designs[as.character(a1), "alpha"] <- temp$alpha
    designs[as.character(a1), "power"] <- temp$power
  }
  invisible(designs)
}