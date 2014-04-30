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
#library(cocor) ## http://www.philippsinger.info/?p=347  http://comparingcorrelations.org/

NULL
#' corlag 
#' 
#' Wrapper function for \code{\link{acf}}
#' 
#' @param x data.frame 
#' @param lag lag-correletion
#' @param ... further arguments for \code{\link{acf}}
#' 
#' 
#' 
#' @export 
#' 

corlag <- function(x,lag=1,...) {
	
	
	out <- array(NA,c(ncol(x),ncol(x)))
	######
	
	out <- acf(x,lag.max=lag,plot=FALSE)$acf[lag+1,,]
#	i <- 1:(nrow(x)-lag)+lag
#	j <- i-lag 
#	
#	
#	for (r in 1:nrow(out)) {
#		for (c in 1:ncol(out)) {
#			
#			out[r,c] <- cor(x=x[i,r],y=x[j,c],...)
#			
#		}
		
#	}
	print(out)
	str(out)

	return(out)
	
	
}


NULL

#'
#' 
#' Function which plots the correlation among observed and generated variables
#'
#' @param x oberserved variable
#' @param y generated variable
#' @param corx correlation of obervations. It must be a \code{NULL} object or a single correlation matrix or a list of correlation matrices.
#' @param use,method see \code{\link{cor}} 
#' @param lag lag for autocorrelation 
#' @param return.just.data.frame logical value. If \code{TRUE} functions returns no plot but just the date frame between observed and modeled autocrosscorrelations. Default is \code{FALSE}. 
#' @param cor.null.value null value for correlation corresponc test. See \code{\link{cocor.indep.groups}} or \url{http://comparingcorrelations.org/}.
#' @param useGPCA integer value. If it is greater than 0, GPCA interations are made in preprocessing. See \link{\code{GPCA}}. Default is 0, no GPCA are preprocessed.
#' @param signif singificance used to plot confindence interval. Default is 0.05 . 
#' @param ... further arguments for eastetics. See \code{\link{aes}} 
#' 
#' 
#' 


corplotlag <- function(x=Tn_mes,y=Tn_gen,corx=NULL,
		xlab="observed",ylab="generated",title="Spatial Correlation",season=FALSE,origin="1960-01-01",return.just.data.frame=FALSE,lag=1,cor.null.value=0,useGPCA=0,signif=0.05,...) {
	
	if (is.data.frame(y)) {
		
		y <- list(y=y)
		names(y) <- ylab[1]
	}
	
	if (season) {
		
		if (is.null(corx)) x <- addseason(x,origin=origin,nodata=TRUE)
		y <- lapply(X=y,FUN=addseason,origin=origin,nodata=TRUE)
		
		
		
		factor <- "season"
		
		factors <- unique(as.character(x[,factor]))
		nobs <- tapply(X=x[,1],INDEX=as.character(x[,factor]),FUN=length)
		
	} else {
		
		factor <- "season"
		nobs <- nrow(x)
		
		factors <- 1
		
##		facors <- factor
		
		
		
	}
	
	if ((useGPCA>0) & season) {
		
		if (is.null(corx))  {
			
			for (it in factors) {
				
				c <- which(as.character(x[,factor])==it)
				
				xg <- GPCA(x_prev=x[c,names(x)!=factor],n=useGPCA,extremes=TRUE)
				x[c,names(x)!=factor] <- xg$final_results[,]
			}
			
		}
		
		y <- lapply(X=y,FUN=function(x,useGPCA,it,factors,factor) {
					
					
					for (it in factors) {
						
						c <- which(as.character(x[,factor])==it)
						
						xg <- GPCA(x_prev=x[c,names(x)!=factor],n=useGPCA,extremes=TRUE)
						x[c,names(x)!=factor] <- xg$final_results[,]
					}
					return(x)
					
					
					
				},useGPCA=useGPCA,it=it,factors=factors,factor=factor)
		
		
		
		
		
	} 
	#### PUT GPCA ON X AND Y !!!!  
	####
	
#	factors <- unique(as.character(x[,factor]))
	
	
#	nobs <- tapply(X=x[,1],INDEX=as.character(x[,factor]),FUN=length)
	
	
	
	
	
	if (is.null(corx)) {
		corx <- fun_factor(data=x,factor=factor,fun=corlag,lag=lag,...)
		
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
	str(corx)
	cory <- lapply(X=y,FUN=fun_factor,factor=factor,fun=corlag,lag=lag,...)
 	str(cory)
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
	
	df$value_max <- df$observed
	df$value_min <- df$observed
	
	### INSERT HERE 
	for (level in unique(as.character(df$level))) {
		
		print(level)
		condlevel  <- as.character(df$level)==level 
		
		for (ifactor in factors) {
	
			no <- nobs[ifactor]
			print(ifactor)
			if (season)  {
				no <- nobs[ifactor]
				print(ifactor)
				condfactor  <- as.character(df[,factor])==ifactor
			} else {
				
				condfactor  <- TRUE
			} 
			
			cond <- condlevel & condfactor
			cond <- which(cond)
			print(cond)
			
			corint <- cor.confidence.interval(r1=df$observed[cond],r2=NA,n=no,signif=signif) 
			
			df$value_max[cond] <- corint$rmax
			df$value_min[cond] <- corint$rmin
			
			
		}
	}
	
	
	df <- df[df$observed!=1 | df$generated!=1,]
	if (return.just.data.frame) return(df)
	asp <- length(unique(df$season))/length(unique(df$level))
############	aes <- aes(x=observed,y=generated,shape=level,group=season,col=level,...)
	out <- qplot(observed,generated, data = df, geom = "point", group = level,method=lm,asp=1) +
			facet_grid(season ~ level, scale = "fixed")+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 

	if (cor.null.value>=0) out <- out+geom_ribbon(mapping=aes(x=observed,ymax=value_max,ymin=value_min),data=df,alpha=0.4)    ### cor.null.value
####  


	
	#	out <- ggplot()+geom_point(mapping=aes,data=df)+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 
#	if (season) out <- out+facet_grid(season~season,scale="fixed")
	return(out)

}
NULL 
#'
#' Function which plots the correlation among observed and generated variables for more lags
#' 
#' @export
#' @seealso \code{\link{corplotlag}}
#' 
corplotlags <- function(x=Tn_mes,y=Tn_gen,lag=c(0,1,4),return.just.data.frame=FALSE,
	xlab="observed",ylab="generated",title="Spatial Correlation",...) {
	
	df <- corplotlag(x=x,y=y,lag=lag[1],return.just.data.frame=TRUE,season=FALSE,...)
	df$lag <- lag[1]
	
	for (i in 2:length(lag)) {
		
		temp <- corplotlag(x=x,y=y,lag=lag[i],return.just.data.frame=TRUE,season=FALSE,...)
		temp$lag <- lag[i]
		
		df <- rbind(df,temp)
		

		
	}
	df$lag <- paste(df$lag,"-day lag",sep="")
	out <- qplot(observed,generated, data = df, geom = "point", group = level,method=lm,asp=1) +
			facet_grid(lag ~ level, scale = "fixed")+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 
	str(df)
	out <- out+geom_ribbon(mapping=aes(x=observed,ymax=value_max,ymin=value_min),data=df,alpha=0.4)    ### cor.null.value
	
	return(out)
}



