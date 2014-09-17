\name{QuantileQuantilePlot}
\alias{QuantileQuantilePlot}
\title{Quantile-Quantile Plot}
\usage{
QuantileQuantilePlot(x, y, qqx = NULL, xlab = "observed",
  ylab = "generated", title = paste("Quantile-Quantile at", station, sep =
  " "), season = FALSE, origin = "1960-01-01", station = "T0090",
  signif = 0.05, name_row = "xxx", ...)
}
\arguments{
  \item{x}{oberserved variable}

  \item{y}{generated variable}

  \item{qqx}{correlation of obervations. It must be a
  \code{NULL} object or a single correlation matrix or a
  list of correlation matrices.}

  \item{signif}{singificance used to plot confindence
  interval. Default is 0.05. (Actually not used!)}

  \item{xlab,ylab,title}{title and axis labels}

  \item{season}{logical value. If \code{TRUE} (default)
  plots are separated per seasons.}

  \item{origin}{date releted of the first row}

  \item{station,name_row}{further arguments}

  \item{...}{further arguments for eastetics. See
  \code{\link{aes}}}
}
\description{
Function which plots the correlation among observed and
generated variables
}
\note{
For confidence intrval see
\url{https://www.stat.auckland.ac.nz/~ihaka/787/lectures-quantiles2-handouts.pdf},
\url{http://stackoverflow.com/questions/19392066/simultaneous-null-band-for-uniform-qq-plot-in-r}
\url{https://support.sas.com/documentation/cdl/en/procstat/63104/HTML/default/viewer.htm#procstat_univariate_sect028.htm#procstat.univariate.clpctl}
Look here for T student
\url{http://en.wikipedia.org/wiki/Student\%27s_t-distribution}
}

