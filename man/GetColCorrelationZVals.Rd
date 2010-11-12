\name{GetColCorrelationZVals}
\alias{GetColCorrelationZVals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Column Correlation Z-values
}
\description{
Gets the Fisher-transformed, standardized column correlations, which should be roughly N(0, 1) under null. 
}
\usage{
GetColCorrelationZVals(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Data matrix.
}
}
\details{
The transformed correlations are multiplied by 0.9 to deal with slightly heavy tails for the test. If you want the unaltered z-values, look at the code or rescale the result.
}
\value{
Vector of z-values (ncol(X) choose 2).
}
\references{
Muralidharan, Omkar. "Detecting column dependence when rows are correlated and estimating the strength of the row correlation". http://stat.stanford.edu/~omkar.}
\author{
Omkar Muralidharan
}
