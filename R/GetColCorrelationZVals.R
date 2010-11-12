GetColCorrelationZVals <-
function(X) {
# Gets Fisher-transformed column correlations
  cc = atanh(cor(X))
  z = cc[row(cc) < col(cc)]
  z = (z - mean(z)) / sd(z)
  z = z * 0.9
  return(z)
}

