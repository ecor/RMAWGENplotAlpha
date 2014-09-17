\name{corplotlags}
\alias{corplotlags}
\title{Function which plots the correlation among observed and generated variables for more lags}
\usage{
corplotlags(x, y, lag = c(0, 1, 4), xlab = "observed", ylab = "generated",
  title = "Spatial Correlation", ...)
}
\arguments{
  \item{x}{e.g. \code{Tn_mes}}

  \item{y}{e.g. \code{Tn_gen}}

  \item{xlab,ylab,title}{title and axis labels}

  \item{lag}{lag for autocorrelation}

  \item{...}{further arguments}
}
\description{
Function which plots the correlation among observed and
generated variables for more lags
}
\seealso{
\code{\link{corplotlag}}
}

