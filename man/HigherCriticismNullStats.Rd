\name{HigherCriticismNullStats}
\alias{HigherCriticismNullStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Higher Criticism Null Statistics
}
\description{
Calculates null statistics for higher criticism test.
}
\usage{
HigherCriticismNullStats(B, lz, max.low, max.high)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{B}{
Number of null statistics to generate.
}
  \item{lz}{
Length of z-value vector (should be the same as the z-value vector you are testing).
}
  \item{max.low}{
Lower endpoint of maximizing interval.
}
  \item{max.high}{
Upper endpoint of maximizing interval.
}
}
\value{
Vector of null statistics.
}
\references{
Donoho, D. & Jin, J.
Higher Criticism for Detecting Sparse Heterogeneous Mixtures
The Annals of Statistics, Institute of Mathematical Statistics, 2004, 32, 962-994}
\author{
Omkar Muralidharan
}
