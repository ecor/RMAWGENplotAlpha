\name{logcor.plot}
\alias{logcor.plot}
\title{logcor.plot}
\usage{
logcor.plot(prec, Tx, Tn, station, origin = "1961-01-01", valmin = 0.5,
  xlab = "xlab", ylab = "ylab", title = "tilte", ...)
}
\arguments{
  \item{prec}{list of precipitation data frames}

  \item{Tx,Tn}{list of daily maximim and minimum
  temperature data frames}

  \item{xlab,ylab,title}{title and axis labels}

  \item{origin}{date corresponding to the first row}

  \item{station}{names of the stations to be used for plot}

  \item{valmin}{minimum accebtable value for precipitation}

  \item{...}{further arguments}
}
\description{
Correlation between log-precipitation and daily temperature
}

