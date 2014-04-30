\name{spellBoxPlot}
\alias{spellBoxPlot}
\title{spellBoxPlot}
\usage{
spellBoxPlot(x = dw.spell.dry.mes, y = dw.spell.dry.gen,
  use.dw.spell = FALSE, field = "spell_length", station = c("T0010",
  "T0090", "T0129"), type = c("P03GPCA", "RMRAINGEN"),
  title = "Dry Spell Duration", extract = "dry", observationIndex = "obs",
  xlab = "type", ylab = "dry spell [day]", season = TRUE, ...)
}
\arguments{
  \item{x}{observed time series}

  \item{y}{generated time series}

  \item{use.dw.spell}{logical value. If \code{TRUE} it uses
  internally \code{\link{dw.spell}}}

  \item{field}{field to be represented}

  \item{station}{stations to be represented in the grid
  columns}

  \item{type}{model type represented on the x following
  observations}

  \item{title,xlab,ylab}{title, labels for the x and y
  axes}

  \item{observationIndex}{chacter string used for
  respresentation of observations or measurements.}

  \item{season}{logical value. If \code{TRUE} the boxplot
  is represeted seasonally.}

  \item{...}{further arguments for
  \code{\link{use.dw.spell}}}
}
\value{
the boxplot of dry or wet spells (Experimental).
}
\description{
It makes Box plot for dry or wet spells
}
\examples{
##
## source('~/Dropbox/iasma/RMAWGENdev/RMAWGEN_article/plottingscripts/ems_article/ccgamma.execution.prec.R', chdir = TRUE)
##
## out <- spellBoxPlot(x=prec_mes,y=prec_gen,use.dw.spell=TRUE,station=c("T0010","T0090","T0129"),origin=origin,title="Dry Spell Duration",extract="dry",observationIndex="obs",xlab="type",ylab="dry spell [day]",season=TRUE)
}

