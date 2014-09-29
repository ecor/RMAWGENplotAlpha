# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL 

#'
#' Function which plots the correlation among observed and generated variables
#'
#' @param x oberserved variable
#' @param y generated variable
#' @param corx correlation of obervations. It must be a \code{NULL} object or a single correlation matrix or a list of correlation matrices.
#' @param return.value string indicating which matrix returned by \code{\link{CCGamma}}. See 
#' @param tolerance,valmin,interval,nearPD see \code{\link{CCGamma}}
#' @param lag lag expressed in days used for the computation in \code{\link{CCGamma}}. It must contain only one value, it must not be a vector.
#' @param xlab,ylab,title title and axis labels
#' @param season logical value. If \code{TRUE} (default) plots are separated per seasons.
#' @param origin date corresponding to the first row
#' @param use see argement entry on \code{\link{cor}}.
#' @param ... further arguments for eastetics. See \code{\link{aes}} 
#' 
#' @export
#' @seealso \code{\link{CCGamma}}
#' 
#' 

ccgammaplot <- function(x,y,use = "everything",corx=NULL,
return.value=c("nooccurence","occurence","continuity_ratio","nooccurence_gcorrelation","nooccurence_correlation"),
lag=0,tolerance=0.0001,
valmin = 0.5,interval = c(-1, 1),nearPD = (lag >= 0),
xlab="observed",ylab="generated",title="Spatial Correlation",season=FALSE,origin="1960-01-01",...) {
	
	if (length(lag)>1) lag <- lag[1]
	if (length(return.value)>1) return.value <- return.value[1]
	if (is.data.frame(y)) {
		
		y <- list(y=y)
		names(y) <- ylab[1]
	}
	
	if (season) {
		
		if (is.null(corx)) x <- addseason(x,origin=origin,nodata=TRUE)
		y <- lapply(X=y,FUN=addseason,origin=origin,nodata=TRUE)
		
		
		factor <- "season"
		
	} else {
		
		factor <- "season"
	}
	
	### CCGAMMA WRAPPER
	
#	ccgamma_wrapper <- function(data,return.value=return.value,lag=lag,tolerance = tolerance,valmin = valmin,interval = interval,nearPD = nearPD) {
#		
#		out <- CCGamma(data=data)
#	}
#	
	
	####
	if (is.null(corx)) {
		corx <- fun_factor(data=x,factor=factor,fun=CCGamma,return.value=return.value,lag=lag,tolerance = tolerance,valmin = valmin,interval = interval,nearPD = nearPD)
	} else if (is.matrix(corx) | is.list(corx)) {
		
		if (is.matrix(corx) | is.data.frame(corx)) {
			corx <- list(corx=corx)
			names(corx) <- factor
		}
		## corx must be a NULL object or a single correlation matrix or a list of correlation matrices 
		out <- lapply(X=corx,FUN=as.vector)
		
		cnt <- unlist(lapply(X=out,FUN=length))
		names(cnt) <- names(out)
		
		names <- rep(names(out),times=cnt)
		
		out <- unlist(out)
		names(out) <- names
		
		n_out <- data.frame(nfactor=names(out),value=out)
		names(n_out) <- c(factor,"value")
		
		corx <- n_out
		
		
	}
	cory <- lapply(X=y,FUN=fun_factor,factor=factor,fun=CCGamma,return.value=return.value,lag=lag,tolerance = tolerance,valmin = valmin,interval = interval,nearPD = nearPD)
##	stop("go on!!!")
	str(corx)
	for (i in names(cory)) {
		
		cory[[i]][,xlab] <- corx$value
		
	}
	
	str(cory)
	cory <- melt(cory,id=c(xlab,factor))
	cory <- cory[,names(cory)!="variable"]
	
	
	names(cory)[names(cory)=="L1"] <- "level"
	
#	str(cory)
	
	names(cory)[names(cory)=="value"] <- "generated"
	names(cory)[names(cory)==xlab] <- "observed"
	
#	str(cory)
#	str(names(cory))
	
	df <- cory
	df <- df[!is.na(df$observed),]
	
	df <- df[df$observed!=1 | df$generated!=1,]
	
	
	
	## mod by ec 20140428
	
	step <- 0.1
	scale_x <- range(df$observed)
	scale_x <- trunc(scale_x/step)*step
	scale_x[2] <- scale_x[2]+step
	###n <- 3
	breaks <- seq(from=scale_x[1],to=scale_x[2],by=step)
	labels <- as.character(breaks)
	nolab <- -((1:(length(breaks)/2))*2)
	labels[nolab] <- ""
	
	
	
	
	
	
	
	############	aes <- aes(x=observed,y=generated,shape=level,group=season,col=level,...)
	out <- qplot(observed,generated, data = df, geom = "point", group = level) +
			facet_grid(season ~ level, scales = "fixed")+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline()+scale_x_continuous(breaks=breaks,labels = labels) ## added on ec 20120427
#	out <- ggplot()+geom_point(mapping=aes,data=df)+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 
#	if (season) out <- out+facet_grid(season~season,scale="fixed")
	return(out)
	
}



