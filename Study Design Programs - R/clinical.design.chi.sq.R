clinical.design.chi.sq <- function(N, fraction.intervention = 2/3,
                                   fraction.intervention.pos.response = 0.8,
                                   fraction.no.intervention.pos.response = 0.6)
{
  number.intervention <- fraction.intervention * N
  number.no.intervention <- (1 - fraction.intervention) * N
  #
  observed.intervention.pos.response <- fraction.intervention.pos.response * number.intervention
  observed.intervention.neg.response <- number.intervention - observed.intervention.pos.response
  observed.no.intervention.pos.response <- fraction.no.intervention.pos.response * number.no.intervention
  observed.no.intervention.neg.response <- number.no.intervention - observed.no.intervention.pos.response
  #
  number.pos <- observed.intervention.pos.response + observed.no.intervention.pos.response
  number.neg <- observed.intervention.neg.response + observed.no.intervention.neg.response
  #
  expected.intervention.pos.response <- number.intervention * number.pos / N
  expected.intervention.neg.response <- number.intervention * number.neg / N
  expected.no.intervention.pos.response <- number.no.intervention * number.pos / N
  expected.no.intervention.neg.response <- number.no.intervention * number.neg / N
  #
  # Calculation via definition
  #
  chi.sq <- 
    (observed.intervention.pos.response - expected.intervention.pos.response)^2/expected.intervention.pos.response +
    (observed.intervention.neg.response - expected.intervention.neg.response)^2/expected.intervention.neg.response +
    (observed.no.intervention.pos.response - expected.no.intervention.pos.response)^2/expected.no.intervention.pos.response +
    (observed.no.intervention.neg.response - expected.no.intervention.neg.response)^2/expected.no.intervention.neg.response
  #
  # Simplified forumula
  #
  chi.sq.1 <- N * (observed.intervention.pos.response * observed.no.intervention.neg.response -
                     observed.intervention.neg.response * observed.no.intervention.pos.response)^2 /
    (number.intervention * number.no.intervention * number.pos * number.neg)
  #
  invisible(list(chi.sq = chi.sq, chi.sq.1 = chi.sq.1, df = 1))
}