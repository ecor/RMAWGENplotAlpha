\name{corplot}
\alias{corplot}
\title{Function which plots the correlation among observed and generated variables}
\usage{
corplot(x, y, use = "everything", corx = NULL, method = c("pearson",
  "kendall", "spearman"), xlab = "observed", ylab = "generated",
  title = "Spatial Correlation", season = FALSE, origin = "1960-01-01",
  ...)
}
\arguments{
  \item{x}{oberserved variable}

  \item{y}{generated variable}

  \item{corx}{correlation of obervations. It must be a
  \code{NULL} object or a single correlation matrix or a
  list of correlation matrices.}

  \item{use,method}{see \code{\link{cor}}}

  \item{xlab,ylab,title}{title and axis labels}

  \item{season}{logical value. If \code{TRUE} (default)
  plots are separated per seasons.}

  \item{origin}{date releted of the first row}

  \item{...}{further arguments for eastetics. See
  \code{\link{aes}}}
}
\description{
Function which plots the correlation among observed and
generated variables
}

