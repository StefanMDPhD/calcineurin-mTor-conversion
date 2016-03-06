sample.sizes <- function(p0 = 0.45, p1 = 0.3, alpha = 0.05, power = 0.8,
                         interim.analyses = c(1/3, 2/3, 1),
                         npm = 5, mmax = 1000)
{
  #
  # Load clinfun, if not previously done
  #
  require(clinfun)
  #
  # Compute sizes of arms using Fisher Exact test (assuming no interim analyses)
  #
  FE <- fe.ssize(p1 = 1 - p0, p2 = 1 - p1, alpha = alpha, power = power, npm = npm, mmax = mmax)
  #
  # Compute sample sizes for group sequential designs
  #
  GS <- gsdesign.binomial(ifrac = interim.analyses, pC = 1 - p0, pE = 1 - p1,
                          sig.level = alpha, power = power, delta.eb = 0.5, delta.fb = 0.5,
                          alternative = "two.sided")
  invisible(list(N = FE["Fisher Exact", "Group 1"], exact.power = FE["Fisher Exact", "Exact Power"],
                 GS = GS))
}