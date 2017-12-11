
# 
# #' @title Goodness of fit
# #'
# #' @description
# #' Functions to assess the agreement between an empirical distribution and a theoretical distribution
# #' 
# #' @param x A numeric vector
# #' @param distr A string indicating the distribution, e.g. \sQuote{norm} for the normal dstribution
# #' @param param A named numeric vector of (estimated) values of parameters of \sQuote{distr}
# #' @param package An optional string indicating the package in which \sQuote{distr} is implemented
# #' @param R An Integer specifying the number of bootstraps
# #' @param method A string specifying the method to be used
# #' @param cdf The CDF of the empirical data
# #'
# #' @return A list with class htest
# #'
# #' @details
# #' Supported methods are Anderson-Darling (\sQuote{\code{anderson.darling}}), Shapiro-Wilks (\sQuote{\code{shapiro.wilks}}), Kolmogorov-Smirnov (\sQuote{\code{ks}}).
# 
# 
# ##' @rdname gof
# ##' @name gof
# #' @aliases gof
# #' @author Markus Boenn
# ##' @export







.gof <- function( x, distr, param, package=NULL, R=100, method="", cdf ){
	param <- as.list(param)
	x <- sort(x)
	N <- length(x)
	
	
	gof2htest <- function(tname, distr, score, p.value){
		method <- gsub( tname, pattern="\\.", replacement=" " )
		method <- paste0( method, " Test for ", sQuote(distr) )
		gof <- list(p.value=as.numeric(p.value), statistic=setNames(score, tname), method=method)
		class(gof) <- "htest"
	return(gof)
	}

	do.cdf <- function( cdf, v, param, lower.tail = TRUE, log.p=TRUE ){
		#print(c(list(v, log.p=log.p, lower.tail=lower.tail), param))
		a <- try(do.call(cdf, c(list(v, log.p=log.p, lower.tail=lower.tail), param) ), silent=TRUE)
		if(!is.numeric(a)){a <- NA}
	return(a)
	}



	if( method %in% c("shapiro.wilks", "sw") ){
		return(shapiro.test(x))
	}
	if( method == "ks" ){
		#print(str(c(list(x=x, y=cdf), param)))
		a <- try(do.call("ks.test", c(list(x=x, y=cdf), param)))
		if(!class(a)=="htest"){a <- NULL}
		return(a)
	}

	do.boot <- function( x ){
		M <- c()
		if(is.matrix(R)){
			M <- R; R <- ncol(R)
		}else{ M <- do.call("cbind", lapply(1:R, function(r){ sample(x, N, replace=TRUE) }) ) }
		
		H <- numeric(R)
		for( r in 1:R ){ 
			H[r] <- calc_score(M[,r])
		}
	return(H)
	}
	
	score <- p.value <- NA
	calc_S <- calc_score <- NULL
	if( method %in% c("anderson.darling", "ad") ){
		calc_S <- function(x){
			S <- (2*(1:N)-1)/N * ( do.cdf(cdf, x[1:N], param) + do.cdf(cdf, x[N+1-(1:N)], param, FALSE) )
			return(sum(S, na.rm=TRUE))
		}
		calc_score <- function(x){ (-N-calc_S(x)) }
	}
	if( method == "cvm" ){
		calc_S <- function(x){
			S <- ( (2*(1:N)-1)/(2*N) - do.cdf(cdf, x[1:N], param, lower.tail=FALSE, log.p=FALSE) )**2; 
			return(sum(S, na.rm=TRUE))
		}
		calc_score <- function( x ){ 1/(12*N) + calc_S(x) }
	}
	if( method == "watson" ){
		calc_S <- function(x){
			S <- ( (2*(1:N)-1)/(2*N) - do.cdf(cdf, x[1:N], param, lower.tail=FALSE, log.p=FALSE) )**2; 
			return(sum(S, na.rm=TRUE))
		}
		calc_score <- function( x ){ 1/(12*N) + calc_S(x) - N*( mean(do.cdf(cdf, x, param, lower.tail=FALSE, log.p=FALSE), na.rm=TRUE) - 0.5 )**2 }
	}

	score <- calc_score(x)
	H <- do.boot(x)
	p.value <- sum( H > score )/ifelse(is.matrix(R), ncol(R), R)
	
	gof <- gof2htest( method, distr, score, p.value )
	
return( gof )
}
