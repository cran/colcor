HigherCriticismStat <-
function(z, max.low, max.high) {
# Given a vector of z-values and a maximizing interval,
# calculates the higher criticism statistic
  e = ecdf(z)
  if(max.high == Inf) max.high = max(z)
  s = seq(max.low, max.high, length = 1000)
  num = 1 - e(s) + e(-s) - 2 * pnorm(-s) # P(|z| > s) - Pnorm(|z| > s)
  denom = 2 * pnorm(-s) * (1 - 2 * pnorm(-s)) / length(s) # binomial variance
  return(max(num / sqrt(denom)))
}

