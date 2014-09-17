# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL 
#' logcor.plot
#' 
#' Correlation between log-precipitation and daily temperature
#' 
#' @param prec list of precipitation data frames 
#' @param Tx,Tn list of daily maximim and minimum temperature data frames
#' @param xlab,ylab,title title and axis labels
#' @param origin date corresponding to the first row
#' @param station names of the stations to be used for plot
#' @param valmin minimum accebtable value for precipitation
#' @param ... further arguments 
#' 
#' @export 
#' 
#' 

## corplotlags
logcor.plot <- function(prec,Tx,Tn,station,origin="1961-01-01",valmin=0.5,xlab="xlab",ylab="ylab",title="tilte",...) {
	sample <- "seasonally"
	extract_cor_value <- function(...) {
		
		out <- cor.test(...)
		conf.int <- out$conf.int 
		estimate <- out$estimate 
		
		out <- c(conf.int[1],estimate,conf.int[2])
		names(out) <- c("min","estimate","max")
		df <- data.frame(value=out,label=names(out))
		out <- df 
		return(out)
		
	}
	
	log_prec <- lapply(X=prec,function(x,valmin){
				x[x<valmin] <- valmin 
				log(x)
			},valmin=0.5)
	
	cors <- applyDailyWeatherFunList(prec=log_prec,Tx=Tx,Tn=Tn,station=station,sample=sample,origin=origin,fun=applyCor,valmin=log(valmin),applyCorfun=extract_cor_value)
	
	
	cors <- melt(cors,id="label")
	names(cors) <- c("label","variable","value","season","station","configuration")
	

	index_gen <- (cors$configuration!="obs"& cors$label=="estimate") 
	
	cors_estimate <- cors[cors$label=="estimate",]
	cors_max <-      cors[cors$label=="max",]
	cors_min <-      cors[cors$label=="min",]
	names(cors_estimate)[names(cors_estimate)=="value"] <- "estimate"
	names(cors_max)[names(cors_max)=="value"] <- "max"
	names(cors_min)[names(cors_min)=="value"] <- "min"

	cors_estimate <- cors_estimate[,names(cors_estimate)!="label"]
	cors_min <- cors_min[,names(cors_min)!="label"]
	cors_max <- cors_max[,names(cors_max)!="label"]
	
	
	str(cors_estimate)
	cors_estimate$max <- cors_estimate$estimate+0.1
	cors_estimate$min <- cors_estimate$estimate-0.1
	
	##
	##
	out <-  qplot(configuration,estimate, data = cors_estimate, geom = "point",shape=configuration,asp=1)
	
	out <- out+facet_grid(season ~ station, scales = "fixed")+xlab(xlab)+ylab(ylab)+ggtitle(title)
	out <- out+geom_linerange(mapping=aes(x=configuration,ymax=max,ymin=min),data=cors_estimate)
	out <- out+scale_x_discrete(labels="")
	return(out)	
	
	
}


NULL 
#' temperature.wetdry.barplot
#' 
#'  Bar plot for  temperature differantiating "dry" or "wet" days.
#' 
#' @param prec precipitation, e. g. \code{prec_gen}
#' @param Tx maximum temperature, e. g. \code{Tx_gen}
#' @param Tn minimum temperature, e. g. \code{Tn_gen}
#' @param station names of the stations to be used for plot 
#' @param origin date of the first row 
#' @param variable variable 
#' @param valmin minimum accebtable value for precipitation
#' @param xlab,ylab,title title and axis labels
#' @param ... further arguments
#' 
#' 
#' @export
temperature.wetdry.barplot <- function(prec,Tx,Tn,station,origin="1961-01-01",valmin=0.5,xlab="xlab",ylab="ylab",title="title",variable,...) {

	sample <- "seasonally"
    if (length(station)>1) station <- station[1]
	
	get.tx <- function(prec,Tx,Tn,valmin_prec=valmin) {
		
		index <- which(prec>valmin_prec)
		print(prec)
		print(Tx)
		out <- list(wet=Tx[index],dry=Tx[-index],all=Tx)
		
		return(out)
		
	}
	get.tn <- function(prec,Tx,Tn,valmin_prec=valmin) {
		
		index <- which(prec>valmin_prec)
		
		out <- list(wet=Tn[index],dry=Tn[-index],all=Tn)
		
		return(out)
		
	}
	get.dt <- function(prec,Tx,Tn,valmin_prec=valmin) {
		
		index <- which(prec>valmin_prec)
		
		out <- list(wet=Tx[index]-Tn[index],dry=Tx[-index]-Tn[-index],all=Tx-Tn)
		
		return(out)
		
	}
	out_tx <- applyDailyWeatherFunList(prec=prec,Tx=Tx_gen,Tn=Tn_gen,station=station,sample=sample,origin=origin,fun=get.tx,valmin=NA) 
	out_tn <- applyDailyWeatherFunList(prec=prec,Tx=Tx_gen,Tn=Tn_gen,station=station,sample=sample,origin=origin,fun=get.tn,valmin=NA) 
	out_dt <- applyDailyWeatherFunList(prec=prec,Tx=Tx_gen,Tn=Tn_gen,station=station,sample=sample,origin=origin,fun=get.dt,valmin=NA) 
	
	out_tx <- melt(out_tx)
	out_tn <- melt(out_tn)
	out_dt <- melt(out_dt)
	
	
	names(out_tx) <- c("value","state","season","configuration") 
	names(out_tn) <- c("value","state","season","configuration") 
	names(out_dt) <- c("value","state","season","configuration") 
	
	
	out_tx$variable <- "temperature_max"
	out_tn$variable <- "temperature_min"
	out_dt$variable <- "thermal_range"
	df <- rbind(out_tx,out_tn,out_dt)
	
	conf <- unique(as.character(df$configuration))
	##df <- df[df$station %in% station,] cocor.indep.groups ## use.dw.spell
	df <- df[df$variable==variable,]
	## Tn_mes
	out <- ggplot(df, aes(x=configuration,y=value))+geom_boxplot()+xlab(xlab)+ylab(ylab)+ggtitle(title)+scale_x_discrete(labels=(1:length(conf)))
	out <- out+facet_grid(season ~ state)
	return(out)
}