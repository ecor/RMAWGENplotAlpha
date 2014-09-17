\name{probabilityFun}
\alias{probabilityFun}
\title{Probibilty density function obtained by a spline interpolation}
\usage{
probabilityFun(x, sample, cf = NULL, density = TRUE, M = 10, dx = 0.01,
  ...)
}
\arguments{
  \item{x,sample,cf,M,dx,...}{arguments of the function}

  \item{density}{logical value}
}
\description{
Probibilty density function obtained by a spline
interpolation
}
\seealso{
\code{ecdf}

x <- rnorm(1000) c <- probabilityFun(x=x,density=FALSE) d
<- probabilityFun(x=x,density=TRUE)
}

