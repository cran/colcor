\name{HigherCriticismStat}
\alias{HigherCriticismStat}
\title{
Higher Criticism Statistic
}
\description{
Calculates Donoho and Jin's higher criticism statistic.
}
\usage{
HigherCriticismStat(z, max.low, max.high)
}
\arguments{
  \item{z}{
z-value vector
}
  \item{max.low}{
Lower endpoint of maximizing interval.
}
  \item{max.high}{
Upper endpoint of maximizing interval.
}
}
\details{
Get the null by simulation (HigherCriticismNullStats).
}
\value{
Higher Criticism statistic.
}
\references{
Donoho, D. & Jin, J. Higher Criticism for Detecting Sparse Heterogeneous Mixtures The Annals of Statistics, 2004, 32, 962-994}
\author{
Omkar Muralidharan
}