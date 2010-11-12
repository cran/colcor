HigherCriticismColDepStat <-
function(X, max.low = abs(qnorm(1/4)),
                                         max.high = abs(qnorm(1 / ncol(X) / (ncol(X) - 1))),
                                         B = 1e4,
                                         getNulls = TRUE,
                                         doubleStandardize = TRUE) {
# Takes a matrix X and tests if the columns of X are dependent
# using higher criticism on the Fisher transformed correlations
# Returns HC statistic, simulated nulls, and p-value
  if(doubleStandardize) X = DoubleStandardize(X)
  z = GetColCorrelationZVals(X)
  hc = HigherCriticismStat(z, max.low, max.high)
  if(getNulls) nulls = HigherCriticismNullStats(B, ncol(X) * (ncol(X) - 1) / 2, max.low, max.high)
  pval = ifelse(getNulls, ecdf(nulls)(hc), NA)
  return(list(hcstat = hc, nulls = ifelse(getNulls, nulls, NA), pval = pval))
}

