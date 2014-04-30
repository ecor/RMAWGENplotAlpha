# TODO: Add comment
# 
# Author: ecor
###############################################################################

NULL
#'
#' Probibilty density function obtained by a spline interpolation 
#'
#'
#'
#' @seealso \code{ecdf}
#' 
#' x <- rnorm(1000)
#' c <- probabilityFun(x=x,density=FALSE)
#' d <- probabilityFun(x=x,density=TRUE)
#' 

probabilityFun <- function(x,sample,cf=NULL,density=TRUE,M=10,dx=0.01,...) {
	
	
	if (is.null(cf)) cf <- ecdf(sample)
	
	MN <- min(sample,na.rm=TRUE)-M
	MX <- max(sample,na.rm=TRUE)+M
	
	sample <- c(MN,sample,MX)
	
	sample <- sort(sample)
	
	y <- cf(sample)
	
	cfs <- approxfun(x=sample,y=y,rule=2)
	
	
	if (density) {
		
		cum1 <- cfs(x-dx)
		cum2 <- cfs(x)
		
		out <- (cum2-cum1)/dx
		
		
		
	} else {
		
	    out <- cfs(x)
	}
	
	 
	return(out)
}


NULL
#'
#' It plots the extremes of a centered confidential band from an empirical distribution given by a sample \code{x}.
#' 
#' @param x sample
#' 
#' 
#' 
#' 
#' @seealso \url{http://www.math.mcgill.ca/~dstephens/OldCourses/556-2006/Math556-Median.pdf}
#' @export
#' @examples
#' 
#' x <- rnorm(10000)
#' out <- sampleConfidenceBand(x,sample=x)
#' 
#' 
#' 
#' 




sampleConfidenceBand <- function(x,sample,signif=0.05,probs=(0:nprobs)/nprobs,nprobs=length(sample)/scale,scale=200,...) {
	
	out <- NULL
	n <- length(sample)
	y <- quantile(sample,probs=probs)
	
	y <- c(y[1],y)
	probs <- c(probs[1],probs)
	
	dy <- diff(y)
	dp <- diff(probs)
	
	dy <- c(dy[1],dy[-1])
	dp <- c(dp[1],dp[-1])
	y <- y[-1]
	
	prob_df <- splinefun(x=y,y=dp/dy,...)## insted of "approxfun"!!
	
	
	cf <- ecdf(sample)(x)
	df <- prob_df(x)
	
	out <- cf*(1-cf)/df^2/sqrt(n)
	
	## FUNCTION FOR STANDARD DEVIATION
	
	
##	diff(sort(x))-(quantile(x,probs=ecdf(x)(x))-x)[-1]
	
#	y <- sort(x)
#	cum <- ecdf(y)
#	y <- c(y[1],y)
#	py <- cum(y)
#	dy <- diff(y)
#	dpy <- diff(py)
	
#	out <- list(cf=cf,df=df,sd=sd)
	
	
##	out <- cum(sortx)
	
	
	return(out)
}



