\name{Alpha2Hat}
\alias{Alpha2Hat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimates mean-squared row correlation
}
\description{
Estimates mean-squared row correlation. Unbiased if columns are independent, positively biased otherwise (bias is approximately the mean-squared column correlation).
}
\usage{
Alpha2Hat(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Data matrix. Function does NOT double standardize, so double standardize it yourself if you need to.
}
}
\details{
}
\value{
Mean-squared column correlation.
}
\references{
Muralidharan, Omkar. "Detecting column dependence when rows are correlated and estimating the strength of the row correlation". http://stat.stanford.edu/~omkar.
}
\author{
Omkar Muralidharan
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
 X = matrix(rnorm(50 * 1000), 1000, 50)
 Alpha2Hat(X)
}
