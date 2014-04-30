\name{corplotlag}
\alias{corplotlag}
\title{Function which plots the correlation among observed and generated variables}
\usage{
corplotlag(x = Tn_mes, y = Tn_gen, corx = NULL, xlab = "observed",
  ylab = "generated", title = "Spatial Correlation", season = FALSE,
  origin = "1960-01-01", return.just.data.frame = FALSE, lag = 1,
  cor.null.value = 0, useGPCA = 0, signif = 0.05, ...)
}
\arguments{
  \item{x}{oberserved variable}

  \item{y}{generated variable}

  \item{corx}{correlation of obervations. It must be a
  \code{NULL} object or a single correlation matrix or a
  list of correlation matrices.}

  \item{use,method}{see \code{\link{cor}}}

  \item{lag}{lag for autocorrelation}

  \item{return.just.data.frame}{logical value. If
  \code{TRUE} functions returns no plot but just the date
  frame between observed and modeled autocrosscorrelations.
  Default is \code{FALSE}.}

  \item{cor.null.value}{null value for correlation
  corresponc test. See \code{\link{cocor.indep.groups}} or
  \url{http://comparingcorrelations.org/}.}

  \item{useGPCA}{integer value. If it is greater than 0,
  GPCA interations are made in preprocessing. See
  \link{\code{GPCA}}. Default is 0, no GPCA are
  preprocessed.}

  \item{signif}{singificance used to plot confindence
  interval. Default is 0.05 .}

  \item{...}{further arguments for eastetics. See
  \code{\link{aes}}}
}
\description{
Function which plots the correlation among observed and
generated variables
}

