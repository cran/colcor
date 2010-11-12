Alpha2Hat <-
function(X) {
# Estimates the mean-squared row correlation
	n = ncol(X)
	N = nrow(X)
	mat = t(X) %*% X
	mat = mat / N
	normnaive = sum(mat ^ 2)
	alphanaive = (normnaive - n) / (n * (n - 1))
	return(-1 / (N - 3) + (N - 1) / (N - 3) * alphanaive)	
}

