DoubleStandardize <-
function(X, niter = 5) {
# Double standardizes a given matrix
  for(i in 1:niter) X = t(scale(t(scale(X))))
  return(X)
}

