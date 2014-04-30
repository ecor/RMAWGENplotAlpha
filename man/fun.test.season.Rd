\name{fun.test.season}
\alias{fun.test.season}
\title{Kolgoromv-Smirnov testing}
\usage{
fun.test.season(x, y, origin = "1960-01-01", station = "T0090",
  origin_x = origin, origin_y = origin, fun.test = ks.test,
  remove.extremes = 0, ...)
}
\arguments{
  \item{x}{}

  \item{y}{}

  \item{origin_x,origin_y,origin}{origin day of the
  datasets. Default is \code{"1960-01-01"}}

  \item{fun.test}{function for test . See
  \code{\link{ks.test}} or \code{\link{wilcox.test}}}

  \item{station}{}

  \item{...}{further arguments for \code{fun.test}}
}
\description{
Kolgoromv-Smirnov testing
}

