\name{temperature.wetdry.barplot}
\alias{temperature.wetdry.barplot}
\title{temperature.wetdry.barplot}
\usage{
temperature.wetdry.barplot(prec, Tx, Tn, station, origin = "1961-01-01",
  valmin = 0.5, xlab = "xlab", ylab = "ylab", title = "title", variable,
  ...)
}
\arguments{
  \item{prec}{precipitation, e. g. \code{prec_gen}}

  \item{Tx}{maximum temperature, e. g. \code{Tx_gen}}

  \item{Tn}{minimum temperature, e. g. \code{Tn_gen}}

  \item{station}{names of the stations to be used for plot}

  \item{origin}{date of the first row}

  \item{variable}{variable}

  \item{valmin}{minimum accebtable value for precipitation}

  \item{xlab,ylab,title}{title and axis labels}

  \item{...}{further arguments}
}
\description{
Bar plot for temperature differantiating "dry" or "wet"
days.
}

