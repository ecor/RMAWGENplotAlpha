

NULL
#' Quantile-Quantile Plot
#' 
#' Function which plots the correlation among observed and generated variables
#'
#' @param x oberserved variable
#' @param y generated variable
#' @param qqx correlation of obervations. It must be a \code{NULL} object or a single correlation matrix or a list of correlation matrices.
#' @param signif singificance used to plot confindence interval. Default is 0.05. (Actually not used!) 
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

QuantileQuantilePlot <- function(x=Tn_mes,y=Tn_gen,qqx=NULL,xlab="observed",ylab="generated",title=paste("Quantile-Quantile at",station,sep=" "),season=FALSE,origin="1960-01-01",station="T0090",signif=0.05,name_row="xxx",...) {
	
	
	
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
	
	

	
	
	for (i in names(y)) {
		
		y[[i]][,xlab] <- x$value
	
	}
	
	####  observation and value (generation) must be of the same length
	
	qqy <- lapply(X=y,FUN=function(x) {
				
				out <- x 
				
				for (it in unique(as.character(x$season))) {
					
					
					
					value <- sort(x$value[as.character(x$season)==it])
					observed <- sort(x$observed[as.character(x$season)==it])
					
					out$value[as.character(out$season)==it] <- value
					out$observed[as.character(out$season)==it] <- observed
				
				}
				
				return(out)
			})
	
	
	

	

		
	
	
	
	
	
	qqy <- melt(qqy,id=c(xlab,factor))
	qqy <- qqy[,names(qqy)!="variable"]
	

	names(qqy)[names(qqy)=="L1"] <- "level"



	names(qqy)[names(qqy)=="value"] <- "generated"
	names(qqy)[names(qqy)==xlab] <- "observed"

	df <- qqy
	df <- df[!is.na(df$observed),]
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
	
############	aes <- aes(x=observed,y=generated,shape=level,group=season,col=level,...)
	out <-  qplot(observed,generated, data = df, geom = "point", asp=1) ## group = level,method=lm,
##	if (!is.na(signif) & (signif>0))  out <- out+geom_ribbon(mapping=aes(x=observed,ymax=value_max,ymin=value_min),data=df,alpha=0.4)    ### group=df$levelcor.null.value
	
	out <- out+facet_grid(season ~ level, scale = "fixed")+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 
	
	return(out)

}
 
NULL
#' Kolgoromv-Smirnov testing  
#' 
#' Kolgoromv-Smirnov testing  
#' 
#' 
#' @param x 
#' @param y 
#' @param origin_x,origin_y,origin origin day of the datasets. Default is \code{"1960-01-01"}
#' @param fun.test function for test . See \code{\link{ks.test}} or \code{\link{wilcox.test}}
#' @param station
#' 
#' @param ... further arguments for \code{fun.test}
#' 
#' 

## USAGE: fun.test.season(Tx_mes,Tx_gen[[4]],station="T0090",fun.test=ks.test,alternative="greater",remove.extremes=0)

fun.test.season <- function(x,y,origin="1960-01-01",station="T0090",origin_x=origin,origin_y=origin,fun.test=ks.test,remove.extremes=0,...) { 


	x <- addseason(data=x,origin=origin_x,nodata=TRUE)
	y <- addseason(data=y,origin=origin_y,nodata=TRUE)
	
	season <- unique(as.character(x$season))
	
	names(season) <- season
	
	out <- lapply(X=season,FUN=function(it,x,y,station,remove.extremes,...){
				
				xs <- as.vector(x[x$season==it,station])
				ys <- as.vector(y[y$season==it,station])
				
				
				if (remove.extremes>0) {
					
					xs <- sort(xs)
					ys <- sort(ys)
					
					lxs <- length(xs)
					lys <- length(ys)
					
					xs <- xs[(remove.extremes+1):(lxs-remove.extremes)]
					ys <- ys[(remove.extremes+1):(lys-remove.extremes)]
					
				}
				
				
				out <- fun.test(x=xs,y=ys,...)
				return(out)
			},x=x,y=y,station=station,remove.extremes=remove.extremes,...)
	


	return(out)

}
#
#
#
#> fun.test.season(Tx_mes-results$Tx_spline,Tx_gen[[2]]-results$Tx_spline,station="T0129")
#$`a) DJF`
#
#Two-sample Kolmogorov-Smirnov test
#
#data:  x[x$season == it, station] and y[y$season == it, station]
#D = 0.0425, p-value = 0.01511
#alternative hypothesis: two-sided
#
#
#$`b) MAM`
#
#Two-sample Kolmogorov-Smirnov test
#
#data:  x[x$season == it, station] and y[y$season == it, station]
#D = 0.0453, p-value = 0.006956
#alternative hypothesis: two-sided
#
#
#$`c) JJA`
#
#Two-sample Kolmogorov-Smirnov test
#
#data:  x[x$season == it, station] and y[y$season == it, station]
#D = 0.0388, p-value = 0.03159
#alternative hypothesis: two-sided
#
#
#$`d) SON`
#
#Two-sample Kolmogorov-Smirnov test
#
#data:  x[x$season == it, station] and y[y$season == it, station]
#D = 0.0267, p-value = 0.2832
#alternative hypothesis: two-sided
#
#
#> 
#
#If y is numeric, a two-sample test of the null hypothesis that x and y were drawn from the same continuous distribution is performed.
#
#Alternatively, y can be a character string naming a continuous (cumulative) distribution function, or such a function. In this case, a one-sample test is carried out of the null that the distribution function which generated x is distribution y with parameters specified by ....
#
#The presence of ties always generates a warning, since continuous distributions do not generate them. If the ties arose from rounding the tests may be approximately valid, but even modest amounts of rounding can have a significant effect on the calculated statistic.
#
#Missing values are silently omitted from x and (in the two-sample case) y.
#
#The possible values "two.sided", "less" and "greater" of alternative specify the null hypothesis that the true distribution function of x is equal to, not less than or not greater than the hypothesized distribution function (one-sample case) or the distribution function of y (two-sample case), respectively. This is a comparison of cumulative distribution functions, and the test statistic is the maximum difference in value, with the statistic in the "greater" alternative being D^+ = max[F_x(u) - F_y(u)]. Thus in the two-sample case alternative = "greater" includes distributions for which x is stochastically smaller than y (the CDF of x lies above and hence to the left of that for y), in contrast to t.test or wilcox.test.
#
#Exact p-values are not available for the two-sample case if one-sided or in the presence of ties. If exact = NULL (the default), an exact p-value is computed if the sample size is less than 100 in the one-sample case and there are no ties, and if the product of the sample sizes is less than 10000 in the two-sample case. Otherwise, asymptotic distributions are used whose approximations may be inaccurate in small samples. In the one-sample two-sided case, exact p-values are obtained as described in Marsaglia, Tsang & Wang (2003) (but not using the optional approximation in the right tail, so this can be slow for small p-values). The formula of Birnbaum & Tingey (1951) is used for the one-sample one-sided case.
#
#If a single-sample test is used, the parameters specified in ... must be pre-specified and not estimated from the data. There is some more refined distribution theory for the KS test with estimated parameters (see Durbin, 1973), but that is not implemented in ks.test.
#
