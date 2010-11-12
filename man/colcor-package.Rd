\name{colcor-package}
\alias{colcor-package}
\alias{colcor}
\docType{package}
\title{Tests for column correlation in the presence of row correlation}
\description{Given a matrix X with possibly correlated rows, tests if the columns of X are correlated. Also estimates the mean-squared row correlation (see paper at http://stat.stanford.edu/~omkar for details)}
\details{
\tabular{ll}{
Package: \tab colcor\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-11-12\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
The function HigherCriticismColDepStat tests for column correlation, and the function Alpha2Hat estimates the mean-squared row correlation. Note that the test double standardizes the data matrix X by default, but Alpha2Hat does not (use DoubleStandardize to double standardize X).
}
\author{
Omkar Muralidharan <omkar@stanford.edu>
}
\references{
Muralidharan, Omkar. "Detecting column dependence when rows are correlated and estimating the strength of the row correlation". http://stat.stanford.edu/~omkar.
}
\keyword{ package }
\keyword{ correlation }

\seealso{
}
\examples{

# Data matrix X
X = matrix(rnorm(50 * 1000), 1000, 50)
r = HigherCriticismColDepStat(X) # X is double-standardized before test
print(r$pval)
Alpha2Hat(DoubleStandardize(X))
}
