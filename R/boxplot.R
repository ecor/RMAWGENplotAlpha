# TODO: Add comment
# 
# Author: ecor
###############################################################################

## http://www.ling.upenn.edu/~joseff/rstudy/summer2010_ggplot2_intro.html
### http://cran.r-project.org/bin/macosx/contrib/3.1

#ggplot(mpg, aes(class, hwy))+
#		stat_boxplot()
#
#equivalent to
# probabilityFun
#ggplot(mpg, aes(class, hwy))+geom_boxplot()
# refernce

NULL
#'spellBoxPlot
#' 
#' It makes Box plot for dry or wet spells
#' 
#' @param x observed time series
#' @param y generated time series 
#' @param use.dw.spell logical value. If \code{TRUE} it uses internally \code{\link{dw.spell}}
#' @param extract see \code{\link{dw.spell}}
#' @param field field to be represented 
#' @param station stations to be represented in the grid columns
#' @param type model type represented on the x following observations
#' @param title,xlab,ylab title, labels for the x and y axes
#' @param observationIndex  chacter string used for respresentation of observations or measurements.
#' @param season logical value. If \code{TRUE} the boxplot is represeted seasonally. 
#' @param ... further arguments for \code{\link{dw.spell}}
#' 
#' 
#' 
#' @return the boxplot of dry or wet spells (Experimental). 
#' 
#' @export
#' 
#' @examples 
#' ## 
#' ## source('~/Dropbox/iasma/RMAWGENdev/RMAWGEN_article/plottingscripts/ems_article/ccgamma.execution.prec.R', chdir = TRUE)
#' ##
#' ## out <- spellBoxPlot(x=prec_mes,y=prec_gen,use.dw.spell=TRUE,station=c("T0010","T0090","T0129"),origin=origin,title="Dry Spell Duration",extract="dry",observationIndex="obs",xlab="type",ylab="dry spell [day]",season=TRUE)

spellBoxPlot <- function(x=dw.spell.dry.mes,y=dw.spell.dry.gen,use.dw.spell=FALSE,field="spell_length",station=c("T0010","T0090","T0129"),type=c("P03GPCA","RMRAINGEN","generated"),title="Dry Spell Duration",extract="dry",observationIndex="obs",xlab="type",ylab="dry spell [day]",season=TRUE,...) {
	
	out <- NULL
	idate <- c("year","month","day")
	if (season) field <- c(field,idate)
	if (is.data.frame(y)) y <- list(generated=y)
 	print(field)
	
	
	if (use.dw.spell) { 
		
		
		x <- dw.spell(x,extract=extract,...) ##x <- dw.spell(prec_mes,extract=extract,...)
		y <- lapply(X=y,FUN=dw.spell,extract=extract,...)## y <- lapply(X=prec_gen,FUN=dw.spell,extract=extract,...)
		
	   ## CALCULATE USE>DW>SPELL
	}
	
	extract_field <- function(x,field) {return(lapply(X=x,FUN=function(t,field) {
							###print(t)
							return(t[,field])},field=field))}

	x <- extract_field(x=x,field=field)
	spell <- lapply(X=y,FUN=extract_field,field=field)
	
    spell[[observationIndex]] <- x 
	type <- c(observationIndex,type)
 ##    print("DA ANDARE VANTI E FINIRE ENTRO LA MATTINATA")
	
	if (season) {
		
		spell <- melt(spell,id=idate)
		str(spell)
		
		
		names(spell) <- c(idate,"variable","value","station","type")
	
		
		spell$variable <- field[!(field %in% idate)]
		
		spell$season <- "season"

		spell$season[spell$month %in% c(12,1,2)] <- "a)DJF"
		spell$season[spell$month %in% c(3,4,5)] <-  "b)MAM"
		spell$season[spell$month %in% c(6,7,8)] <-  "c)JJA"
		spell$season[spell$month %in% c(9,10,11)] <-"d)SON"
		
		
	} else {	
		spell <- melt(spell)
		str(spell)
		names(spell) <- c("variable","value","station","type")
		spell$variable <- field[!(field %in% idate)]
	}
	
	spell <- spell[spell$station %in% station,] 
	spell <- spell[spell$type %in% type,]
#	print(unique(spell$type))
#	stop()
	str(spell)
	
	str(spell)
	## temporarary 
	
	
	out <- ggplot(spell, aes(x=type,y=value))+geom_boxplot()+xlab(xlab)+ylab(ylab)+ggtitle(title)
	
	if (season) out <- out+facet_grid(season ~ station)
	return(out)
}



