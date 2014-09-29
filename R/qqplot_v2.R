####### observed

NULL
#' Quantile-Quantile Plot
#' 
#' Function which plots the correlation among observed and generated variables
#'
#' @param x oberserved variable
#' @param y generated variable
#' @param qqx correlation of obervations. It must be a \code{NULL} object or a single correlation matrix or a list of correlation matrices.
#' @param signif singificance used to plot confindence interval. Default is 0.05. (Actually not used!) 
#' @param xlab,ylab,title title and axis labels
#' @param season logical value. If \code{TRUE} (default) plots are separated per seasons.
#' @param origin date releted of the first row
#' @param station,name_row  further arguments
#' @param ... further arguments for eastetics. See \code{\link{aes}} 
#' 
#' 
#' 
#' @note For confidence intrval see \url{https://www.stat.auckland.ac.nz/~ihaka/787/lectures-quantiles2-handouts.pdf}, \url{http://stackoverflow.com/questions/19392066/simultaneous-null-band-for-uniform-qq-plot-in-r}
#' \url{https://support.sas.com/documentation/cdl/en/procstat/63104/HTML/default/viewer.htm#procstat_univariate_sect028.htm#procstat.univariate.clpctl}
#' Look here for T student \url{http://en.wikipedia.org/wiki/Student\%27s_t-distribution}
#' 
#' @export
#' 

QuantileQuantilePlot <- function(x,y,qqx=NULL,xlab="observed",ylab="generated",title=paste("Quantile-Quantile at",station,sep=" "),season=FALSE,origin="1960-01-01",station="T0090",signif=0.05,name_row="xxx",...) {
	
	
	
	if (is.data.frame(y)) {
		
		y <- list(y=y)
		names(y) <- ylab[1]
	}
	
	
	x <- data.frame(value=x[,station])
	
	y <- lapply(X=y,FUN=function(x,station) {data.frame(value=x[,station])},station=station)

	
	if (season) {
		
		if (is.null(qqx)) x <- addseason(x,origin=origin,nodata=TRUE)
		y <- lapply(X=y,FUN=addseason,origin=origin,nodata=TRUE)
		
		
		factor <- "season"
		
	} else {
		
		factor <- "season"
		if (is.null(qqx)) x[,factor] <- name_row
		y <- lapply(X=y,FUN=function(x,name_row) {x[,factor]<- name_row;return(x)},name_row=TRUE)

	}
	
	# 
	# 
	#
	#
	#
	#
	#

	
	
	for (i in names(y)) {
		
		y[[i]][,xlab] <- x$value
	
	}
	
	
	#str(x)
	#xx <<- x
	#stop()
	
	####  observation and value (generation) must be of the same length
	
#	qqy <- lapply(X=y,FUN=function(x) {
#				
#				out <- x 
#				
#				for (it in unique(as.character(x$season))) {
#					
#					
#					
#					value <- sort(x$value[as.character(x$season)==it])
#					observed <- sort(x$observed[as.character(x$season)==it])
#					
#					## FROM qqplot function source code 
#					sx <- value
#					sy <- observed
#					##
#					
#					
#					##
#					lenx <- length(sx)
#					leny <- length(sy)
#					
#					if (leny < lenx) 
#						sx <- approx(1L:lenx, sx, n = leny)$y
#					if (leny > lenx) 
#						sy <- approx(1L:leny, sy, n = lenx)$y
#					##
#					value <- sx[1:lenx]
#					observed <- sy[1:leny]
#				
#					## END
#					
#					out$value[as.character(out$season)==it] <- value
#					out$observed[as.character(out$season)==it] <- observed
#				
#				}
#				str(out)
#				outt <<- out
#			
#				return(out)
#			})
	
	
	

	

		
	
	
	
	
	qqy <- y
	qqy <- melt(qqy,id=c(xlab,factor))
	qqy <- qqy[,names(qqy)!="variable"]
	

	names(qqy)[names(qqy)=="L1"] <- "level"



	names(qqy)[names(qqy)=="value"] <- "generated"
	names(qqy)[names(qqy)==xlab] <- "observed"

	df <- qqy
	
#	str(df)
#	dff <<- df
#	stop()
	idvect <- paste(as.character(df$level),as.character(df$season),sep="__")
	for (it in unique(idvect)) { 
	 
		print(it)
		x <- df$observed[idvect==it]
		y <- df$generated[idvect==it]
		
		lenxo <- length(x)
		lenyo <- length(y)
		
		sx <- sort(x)
		sy <- sort(y)
		lenx <- length(sx)
		leny <- length(sy)
		if (leny < lenx)sx <- approx(1L:lenx, sx, n = leny)$y
		if (leny > lenx)sy <- approx(1L:leny, sy, n = lenx)$y
		
		
		df$observed[idvect==it] <- sx[1:lenxo]
		df$generated[idvect==it] <- sy[1:lenyo]
		
	}	
	
	
	####df <- df[!is.na(df$observed),]
	if (!is.na(signif) & (signif>0)) { 
		
		
		idvect <- paste(as.character(df$level),as.character(df$season),sep="__") ## all fileds of df except "observed" and "generated"
		
		df$value_min <- df$observed
		df$value_max <- df$observed
		
		for (it in unique(idvect)) {
			print(it)
			print(unique(idvect))
			index <- which(idvect %in% it)
			str(index)
			ci <- 5.0
			df$value_min[index] <- as.vector(df$observed[index])-ci
			df$value_max[index] <- as.vector(df$observed[index])+ci
			
		}
	#	str(df)
	#	str(idvect)
	#	stop()
	
	
	}
	
	
	
	
###	df <- df[df$observed!=1 | df$generated!=1,]
	
############	aes <- aes(x=observed,y=generated,shape=level,group=season,col=level,...) ## fun_factor
	out <-  qplot(observed,generated, data = df, geom = "point", asp=1) ## group = level,method=lm,
##	if (!is.na(signif) & (signif>0))  out <- out+geom_ribbon(mapping=aes(x=observed,ymax=value_max,ymin=value_min),data=df,alpha=0.4)    ### group=df$levelcor.null.value
	
	out <- out+facet_grid(season ~ level, scales = "fixed")+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 
	
	return(out)

}
 