# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL

#' cor.diff.test
#' 
#' cor.diff.test description
#' 
# http://comparingcorrelations.org/
#' 
#' 

#' @references \url{http://stackoverflow.com/questions/14519006/significance-test-on-the-difference-of-two-correlation-coefficient}
#cocor - comparing correlations, 1.0-0, http://comparingcorrelations.org
#
#INPUT:
#		cocor.indep.groups(r1.jk=0.60, r2.hm=0.65, n1=2700, n2=2700, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
#
#OUTPUT:
#		Results of a comparison of two correlations based on independent groups
#
#Comparison between r1.jk = 0.6 and r2.hm = 0.65
#Group sizes: n1 = 2700, n2 = 2700
#Null hypothesis: r1.jk is equal to r2.hm
#Alternative hypothesis: r1.jk is not equal to r2.hm (two-sided)
#Alpha: 0.05
#
#fisher1925: Fisher's z (1925)
#		z = -3.0168, p-value = 0.0026
#		Null hypothesis rejected
#		
#		zou2007: Zou's (2007) confidence interval
#95% confidence interval for r1.jk - r2.hm: -0.0826 -0.0175
#		Null hypothesis rejected (Interval does not include 0)
#
#

cor.diff.test <-  function(r1, r2, n1, n2, alternative = c("two.sided", "less", "greater")) {
	
	Z1 = 0.5 * log( (1+r1)/(1-r1) )
	Z2 = 0.5 * log( (1+r2)/(1-r2) )
	
	diff = Z1 - Z2
	SEdiff = sqrt( 1 / (n1 - 3) + 1 / (n2 - 3))
	diff.Z = diff / SEdiff
	
	if (alternative == "less") {
		return(pnorm(diff.Z, lower.tail=FALSE))
	} else if (alternative == "greater") {
		return(pnorm(-diff.Z, lower.tail=FALSE))
	} else if (alternative == "two.sided") {
		return(2 * pnorm( abs(diff.Z), lower.tail=FALSE))
	} else {
		warning(paste("Invalid alterantive", alternative), domain=NA)
		return(NA)
	}
}



