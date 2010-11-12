\name{HigherCriticismColDepStat}
\alias{HigherCriticismColDepStat}
\title{
Test for column correlation.
}
\description{
Tests for column correlation when rows are possibly correlated.
}
\usage{
HigherCriticismColDepStat(X, max.low = abs(qnorm(1/4)), max.high = abs(qnorm(1/ncol(X)/(ncol(X) - 1))), B = 10000, getNulls = TRUE, doubleStandardize = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Data matrix.
}
  \item{max.low}{
Lower endpoint for higher criticism maximization.
}
  \item{max.high}{
Upper endpoint for higher criticism maximization.
}
  \item{B}{
Number of null datasets to generate.
}
  \item{getNulls}{
Generate nulls and get p-value?
}
  \item{doubleStandardize}{
Double standardize before testing?
}
}
\details{
To view the column correlation z-values, use GetColCorrelationZVals (double standardize the matrix yourself before using that function).
}
\value{
\item{hcstat}{Higher Criticism test statistic}
\item{nulls}{Null statistics}
\item{pval}{p-value for column independence}
}
\references{
Muralidharan, Omkar. "Detecting column dependence when rows are correlated and estimating the strength of the row correlation". http://stat.stanford.edu/~omkar.
}
\author{
Omkar Muralidharan
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
X = matrix(rnorm(50 * 1000), 1000, 50)
hc = HigherCriticismColDepStat(X)
}