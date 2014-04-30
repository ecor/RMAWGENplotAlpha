\name{ccgammaplot}
\alias{ccgammaplot}
\title{Function which plots the correlation among observed and generated variables}
\usage{
ccgammaplot(x = prec_mes, y = prec_gen, use = "everything", corx = NULL,
  return.value = c("nooccurence", "occurence", "continuity_ratio",
  "nooccurence_gcorrelation"), lag = 0, tolerance = 1e-04, valmin = 0.5,
  interval = c(-1, 1), nearPD = (lag >= 0), xlab = "observed",
  ylab = "generated", title = "Spatial Correlation", season = FALSE,
  origin = "1960-01-01", ...)
}
\arguments{
  \item{x}{oberserved variable}

  \item{y}{generated variable}

  \item{corx}{correlation of obervations. It must be a
  \code{NULL} object or a single correlation matrix or a
  list of correlation matrices.}

  \item{return.value}{string indicating which matrix
  returned by \code{\link{CCGamma}}. See}

  \item{tolerance,valmin,interval,nearPD}{see
  \code{\link{CCGamma}}}

  \item{lag}{lag expressed in days used for the computation
  in \code{\link{CCGamma}}. It must contain only one value,
  it must not be a vector.}

  \item{...}{further arguments for eastetics. See
  \code{\link{aes}}}
}
\description{
Function which plots the correlation among observed and
generated variables
}
\seealso{
\code{\link{CCGamma}}
}

