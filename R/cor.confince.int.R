# TODO: Add comment
# 
# Author: ecor
###############################################################################

NULL
#'
#' This calcultets the confidence interval 
#' 
#' @param r1,r2 correltion coefficients of the two paired samples
#' @param n length (size) of the two paired samples
#' @param signif significance
#' 
#' 
#' @export
#' @seealso \url{http://onlinestatbook.com/lms/estimation/correlation_ci.html} or \url{http://en.wikipedia.org/wiki/Fisher_transformation}

cor.confidence.interval <- function(r1,r2=NA,n,signif=0.05) {
	
	if (n<4) n<- 4 ### WARNING
	rmax <- 0.9999
	r1[r1<(-rmax)] <- -rmax
	r2[r2<(-rmax)] <- -rmax
	
	r1[r1>rmax] <- rmax
	r2[r2>rmax] <- rmax
	
	z1 <- atanh(r1)
	
	z2 <- atanh(r2)
	
	if(is.na(r2[1])) {
		zm <- z1
	
	} else {
		
		zm <- (z1+z2)/2
	
	}
	
	mean <- zm
	sd <- (n-3)^-0.5
	
	ppm <- pnorm(zm,mean=mean,sd=sd)
	
	zmax <- qnorm(ppm+(1-signif)/2.0,mean=mean,sd=sd)
	zmin <- qnorm(ppm-(1-signif)/2.0,mean=mean,sd=sd)
	
	rmax <- tanh(zmax)
	rmin <- tanh(zmin)
	
	out <- list(rmax=rmax,rmin=rmin)
	
	return(out)
	
}
