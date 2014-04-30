\name{sampleConfidenceBand}
\alias{sampleConfidenceBand}
\title{It plots the extremes of a centered confidential band from an empirical distribution given by a sample \code{x}.}
\usage{
sampleConfidenceBand(x, sample, signif = 0.05, probs = (0:nprobs)/nprobs,
  nprobs = length(sample)/scale, scale = 200, ...)
}
\arguments{
  \item{x}{sample}
}
\description{
It plots the extremes of a centered confidential band from
an empirical distribution given by a sample \code{x}.
}
\examples{
x <- rnorm(10000)
out <- sampleConfidenceBand(x,sample=x)
}
\seealso{
\url{http://www.math.mcgill.ca/~dstephens/OldCourses/556-2006/Math556-Median.pdf}
}

