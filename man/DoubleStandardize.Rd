\name{DoubleStandardize}
\alias{DoubleStandardize}
\title{
Double Standardize
}
\description{
Double standardizes matrix by successive row and column scaling
}
\usage{
DoubleStandardize(X, niter = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Data matrix
}
  \item{niter}{
Number of row/col standardizations. 
}
}
\details{
}
\value{
Double standardized matrix.
}
\author{
Omkar Muralidharan
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(X, niter = 5) {
# Double standardizes a given matrix
  for(i in 1:niter) X = t(scale(t(scale(X))))
  return(X)
  }
}