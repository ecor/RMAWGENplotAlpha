# TODO: Add comment
# 
# Author: ecor
###############################################################################
# file corplot.R
#
# This file ...
#
# author: Emanuele Cordano on 17-01-2014
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################


#library(ggplot2)
#library(reshape2)
#library(RMAWGEN)
#fun.test.season
NULL
#' addseson  
#' 
#' Adding season attribute to a data frame 
#' 
#' @param data data frame
#' @param origin Origin Date as a string 
#' @param nodata logical value
#' 
#' 
#' 
#' @export
#' 
#' 

addseason <- function(data,origin,nodata=TRUE) {
	
	
	winter <- c(12,1,2) #c("Dec","Jan","Feb")
	spring <- c(3,4,5)  #c("Mar","Apr","May")
	summer <- c(6,7,8)  #c("Jun","Jul","Aug")
	autumn <- c(9,10,11) #c("Sep","Oct","Nov")
	
	
	if ("month" %in% names(data)) {
		
		out <- out
		
	} else {
		
		out <- adddate(data,origin=origin)
	}
	
	
	
	season <- array("noseason",length(data$month))
	season[out$month %in% winter] <- "a) DJF"	
	season[out$month %in% spring] <- "b) MAM"
	season[out$month %in% summer] <- "c) JJA"
	season[out$month %in% autumn] <- "d) SON"
	
	out$season <- factor(season)
	if (nodata) out <- out[c("season",names(data))]
	
	return(out)
	
}


NULL
#'
#' Applies a function to a data frame according to a FACTOR field. 
#' 
#' @param data input dataset as a data.frame class
#' @param factor field of \code{data} which is a factor.
#' @param fun function
#' @param returns.data.frame logical value. Default is \code{TRUE}.
#' @param ... further arguments or \code{fun}
#' 
#' 
#' 
#' @export
#' 
#' 


fun_factor <- function(data,factor=NA,fun,returns.data.frame=TRUE,...) {
	
	
	
	
	if (is.na(factor) | is.null(factor)) {
		
		nfactor <- "NA"
		factor <- factor(array(nfactor,nrow(data)))
		df <- data

		
	} else if (factor %in% names(data)) {
		nfactor <- factor
		factor <- data[,factor]
		df <- data[,!(names(data) %in% factor)]
		
	} else {
		
		nfactor <- factor
		factor <- factor(array(nfactor,nrow(data)))
		df <- data
	}
	
	out <- list()
	
	for (it in levels(factor)) {
		
		out[[it]] <- df[factor==it,names(df)!=nfactor]
		
		
	}
	
	out <- lapply(X=out,FUN=fun,...)
	
	if (returns.data.frame) {
		
		out <- lapply(X=out,FUN=as.vector)
		
		
		cnt <- unlist(lapply(X=out,FUN=length))
		names(cnt) <- names(out)
		
		names <- rep(names(out),times=cnt)
		
		out <- unlist(out)
		names(out) <- names
		
		n_out <- data.frame(nfactor=names(out),value=out)
		names(n_out) <- c(nfactor,"value")
		
		out <- n_out
		
		
		
	}
		
		
	
	
	return(out)
	
	
	
	
	
	
	
}

## fun.test.season 

#'
#' Function which plots the correlation among observed and generated variables
#'
#' @param x oberserved variable
#' @param y generated variable
#' @param corx correlation of obervations. It must be a \code{NULL} object or a single correlation matrix or a list of correlation matrices.
#' @param use,method see \code{\link{cor}} 
#' @param xlab,ylab,title title and axis labels
#' @param season logical value. If \code{TRUE} (default) plots are separated per seasons.
#' @param origin date releted of the first row
#' @param ... further arguments for eastetics. See \code{\link{aes}} 
#' 
#' @export
#' 


corplot <- function(x,y,use = "everything",corx=NULL,
		method = c("pearson", "kendall", "spearman"),xlab="observed",ylab="generated",title="Spatial Correlation",season=FALSE,origin="1960-01-01",...) {
	
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
	
	
	if (is.null(corx)) {
		corx <- fun_factor(data=x,factor=factor,fun=cor,use=use,method=method)
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
	cory <- lapply(X=y,FUN=fun_factor,factor=factor,fun=cor,use=use,method=method)
 
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
	
	
	
	
	
	
	
	
	
	
	
############	aes <- aes(x=observed,y=generated,shape=level,group=season,col=level,...)
	out <- qplot(observed,generated, data = df, geom = "point", group = level) +
			facet_grid(season ~ level, scales = "fixed")+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 
#	out <- ggplot()+geom_point(mapping=aes,data=df)+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 
#	if (season) out <- out+facet_grid(season~season,scale="fixed")
	return(out)

}

#### UNCOUPLED GENERATION
#
## http://stackoverflow.com/questions/11800212/write-plots-to-png-file ### LOOK AT HERE ... 
#wpath <-  '/Users/ecor/Dropbox/iasma/RMAWGENdev/RMAWGEN/inst/doc/private/elsevier/article/Rscript_3/RMAWGENCodeCorner/output_cassiopea/temperature_generator_multi_station'
#rdaname <-  'results_uncoupled_temperature_generator_plural_station.rda'
#
#rdaname <- paste(wpath,rdaname,sep='/')
#
#
#load(rdaname)
#
#winter <- c("Dec","Jan","Feb")
#spring <- c("Mar","Apr","May")
#summer <- c("Jun","Jul","Aug")
#autumn <- c("Sep","Oct","Nov")
#
#
#
#
#
#year_min <- results$Tx_mes
#year_min <- results$Tx_mes
#
#
#Tx_mes <- results$Tx_mes-results$Tn_spline
#Tn_mes <- results$Tn_mes-results$Tx_spline
#
#
#Tx_gen <- lapply(X=results$Tx_gen,FUN=function(x,y){x-y},y=results$Tn_spline)
#Tn_gen <- lapply(X=results$Tn_gen,FUN=function(x,y){x-y},y=results$Tx_spline)
#
#method <- "kendall"
#
### plot Temparature Correletion 
#pdf <- paste(wpath,'spatial_correlation_tmin_uncoupled.pdf',sep='/')
#
#
#corplot_tn <- corplot(x=Tn_mes,y=Tn_gen,method=method,season=TRUE,title="Spatial Correlation: daily minimum temperature anom.")
#
#pdf(pdf)
#print(corplot_tn)
#dev.off()
#
#pdf <- paste(wpath,'spatial_correlation_tmax_uncoupled.pdf',sep='/')
#
#
#corplot_tx <- corplot(x=Tx_mes,y=Tx_gen,method=method,season=TRUE,title="Spatial Correlation: daily maximum temperature anom.")
#
#pdf(pdf)
#print(corplot_tx)
#dev.off()
#
#
#pdf <- paste(wpath,'spatial_correlation_tmin_uncoupled_unseas.pdf',sep='/')
#
#
#corplot_tn_unseas <- corplot(x=Tn_mes,y=Tn_gen,method=method,season=FALSE,title="Spatial Correlation: daily minimum temperature anom.")
#
#pdf(pdf)
#print(corplot_tn_unseas)
#dev.off()
#
#pdf <- paste(wpath,'spatial_correlation_tmax_uncoupled_unseas.pdf',sep='/')
#
#
#corplot_tx_unseas <- corplot(x=Tx_mes,y=Tx_gen,method=method,season=FALSE,title="Spatial Correlation: daily maximum temperature anom.")
#
#pdf(pdf)
#print(corplot_tx_unseas)
#dev.off()
#
### http://stackoverflow.com/questions/11800212/write-plots-to-png-file

