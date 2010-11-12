HigherCriticismNullStats <-
function(B, lz, max.low, max.high) {
# Given maximizing interval,
# Returns B higher criticism statistics, 
# each calculated on a N(0, 1) vector of length lz
  Z = matrix(rnorm(B * lz), lz, B)
  Z = Z - colMeans(Z)
  Z = Z / sqrt(colMeans(Z ^ 2))
  hc = apply(Z, 2, HigherCriticismStat, max.low, max.high)
  return(hc)
}

