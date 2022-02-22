


#' @title Fit distributions to empirical data
#'
#' @description
#' Fits theoretical univariate distributions from the R universe to a given set of empirical observations
#' 
#' @param X A numeric vector
#' @param dom A string specifying the domain of \sQuote{X}
#' @param freq The frequency of values in \sQuote{X}. See details.
#' @param R An integer specifying the number of bootstraps. See details.
#' @param timeout An numeric value specifying the maximum time spend for a fit
#' @param posList A list. See details.
#' @param fast A logical. See details.
#'
#' @return A list serving as an unformatted report summarizing the fitting.
#'
#' @details
#' This routine is the workhorse of the package. It takes empirical data and systematically tries to fit numerous distributions implemented in R packages to this data.
#' Sometimes the empirical data is passed as a histogram. In this case \sQuote{X} takes the support and \sQuote{freq} takes the number of occurences of each value in \sQuote{X}. Although not limited to, this makes most sense for discrete data. 
#' If there is prior knowledge (or guessing) about candidate theoretical distributions, these can be specified by \sQuote{posList}. This parameter takes a list with names of items being the package name and items being a character vector containing names of the distribtions (with prefix 'd'). If all distributions of a package should be applied, this vector is set to \code{NA}.
#' Fitting of some distributions can be very slow. They can be skipped if \sQuote{fast} is set to \code{TRUE}.
#' 
#' 
#' @note
#' To reduce the computational efforts, usage of the parameter \sQuote{posList} is recommended. If not specified, the function will try to perform fits to distributions from _ALL_ packages listed in \code{\link{supported.packages}}.
#'
#' @seealso
#' \code{\link{printReport}} for post-processing of all fits
#' 
#' @examples
#' # continous empirical data
#' x <- rnorm(1000, 50, 3)

#' if(requireNamespace("ExtDist")){
#' r <- fitter(x, dom="c", posList=list(stats=c("dexp"), ExtDist=c("dCauchy")))
#' }else{
#' r <- fitter(x, dom="c", posList=list(stats=c("dexp", "dt")))
#' }
#'
#' # discrete empirical data
#' x <- rnbinom(100, 0.5, 0.2)
#' r <- fitter(x, dom="dis", posList=list(stats=NA))

#'
#' @aliases fitter
#' @name fitter
#' @rdname fitter
#' @author Markus Boenn
#' @export



fitter <- function(X, dom="discrete", freq=NULL, R=100, timeout=5, posList=NULL, fast=TRUE){

	if(!is.null(freq)){
		X <- rep(X, times=freq)
	}
	
	dom <- match.arg(dom, c("discrete", "continous"))

	##%% evaluate X
	N <- length(X)
	x_range <- range(X)
	x_min <- x_range[1]
	x_max <- x_range[2]
	x_mean <- mean(X)
	x_median <- median(X)
	x_sd <- sd(X)
	x_disp_nbinom <- x_mean**2 /( x_sd**2 - x_mean ) 
	aux_s <- log(x_mean)-mean(log(X)) 
	x_scale_gamma <- ( 3-aux_s+sqrt( (aux_s-3)**2 + 24*aux_s ) ) / (12*aux_s) # k
	x_shape_gamma <- x_mean/x_scale_gamma # theta
	x_skew <- (x_mean-x_median)/x_sd 

	if(is.nan(x_disp_nbinom) || x_disp_nbinom<0){ x_disp_nbinom <- NaN }
	x_emp <- setNames( 
		c(N, x_min, x_max, x_mean, x_median, x_sd, x_scale_gamma, x_disp_nbinom, x_shape_gamma, x_skew), 
		c("N", "min", "max", "mean", "median", "sd", "scale_gamma", "dispersion_nbinom", "shape_gamma", "skew") 
	)
	##%%
	
	dofit <- function(x, AllStarts, Package, dst, constraints=NULL, add=NULL){
		message("Fitting ", sQuote( paste0(Package, "::", dst) ))
		nms <- colnames(AllStarts); inms <- paste0("i.", nms)

		foo <- get(dst, asNamespace(Package))
		dens <- function(param){
			sum( do.call( foo, c(list(x), as.list( c( param, log=TRUE ) ) ) ) )
		}
		if( Package == "ExtDist" ){
			foo <- get(sub(dst, pattern="^d", replacement="l"), asNamespace(Package))
			dens <- function(param){
				sum( do.call( foo, c(list(x), as.list( c( param ) ) ) ))
			}
		}

		OUT <- c()
		myNA <- setNames( rep(NA, length(nms)), nms )
		for( asc in 1:nrow(AllStarts) ){
			start <- setNames( AllStarts[asc,], nms )
			for( useConstraints in c(TRUE, FALSE) ){
				
				
				timeStart <- as.numeric(format(Sys.time(), "%s%OS6"))
				
				constraints0 <- NULL; if(useConstraints){ constraints0 <- constraints }
				param.maxLik <- withTimeout(suppressWarnings( try({ maxLik( dens, start=start, constraints=constraints0 ) }, silent=TRUE)), timeout=timeout, onTimeout="warning")
				
				estim <- myNA
				if( is.character(param.maxLik) || is.null(param.maxLik) || param.maxLik$iterations == 0 ){  
					;
				}else{
					estim <- param.maxLik$estimate
				}
				out <- c(estim, setNames( start, inms ), useConstraints=useConstraints, rt=as.numeric(format(Sys.time(), "%s%OS6"))-timeStart)
				OUT <- rbind(OUT, out)
			}
		}
		
	return(OUT)
	}
	
	fit2report <- function(x, fits, Package, dst, meta){
		message("Reporting ", sQuote( paste0(Package, "::d", dst) ))

		nargs <- meta$nargs
		RPT <- list( Package=Package, distr=dst, nargs=meta$nargs, args=meta$args, fits=fits )
		if( is.null(fits) ){ return(RPT) }
		
		gofMethods <- c("ks", "sw", "ad") #, "cvm", "sw", "watson")
		p.gofMethods <- paste0("p.", gofMethods)
		G <- length(gofMethods)
		gof <- matrix( NA, nrow=nrow(fits), ncol=length(gofMethods)*2, dimnames=list( c(), sapply(1:G, function(i){ c(gofMethods[i], p.gofMethods[i]) }) ) )
		M <- do.call("cbind", lapply(1:100, function(r){ sample(x, length(x), replace=TRUE) }) )
		
		foo <- get(paste0("p", dst), asNamespace(Package))
		for( f in 1:nrow(fits) ){
			fit <- fits[f,]
			if(is.na(fit[1])){next;}
			for( g in 1:G ){
				m <- gofMethods[g]
				tmp <- .gof( x, dst, fit[1:nargs], package=Package, R=M, method=m, cdf=foo )
				#if(!is.null(tmp) && class(tmp)=="htest"){
				if(!is.null(tmp) && inherits(tmp, what=c("htest"))){
					gof[f, c(m, p.gofMethods[g]) ] <- c(tmp[["statistic"]], tmp[["p.value"]])
				}
			}
		}
		RPT[["gof"]] <- gof
	return(RPT)
	}

	
	##%% read meta table
	domtype <- NULL
	META <- subset(META1, domtype==dom)
	META <- split(META, META[, "package"])
	
	REPORT <- list( date=date(), domain=dom, meta=META1, x=sort(X), ecdf=ecdf2(X), emp=x_emp )
	REPORT0 <- list(); r0cnt <- 1
	LS <- ls(); 
	for( i in 1:length(META) ){
		Package <- names(META)[i]
		if( Package == "truncnorm" ){ next; } # skip because no parameter 'log' implemented (required to solve logLikelihood)
		if( !is.null(posList) ){ if(!(Package %in% names(posList))){next;} }
		
		if( !requireNamespace( Package ) ){ message("Package ", Package, " not installed. Skipping."); next; }
		
		for( j in 1:nrow(META[[i]]) ){
			meta <- META[[i]][j, ]
			
			dst <- meta[["fun"]] # name of distribution with leading 'd'
		
			##%% check for packages and distributions to be skipped
			if( fast ){
				if( Package %in% names(slowPack) && dst %in% slowPack[[Package]] ){ next; }
			}
			if( !is.null(posList) ){
				if( !is.na(posList[[Package]]) && !(dst %in% posList[[Package]]) ){ next; }
			}
			##%%

			nargs <- meta[["nargs"]]
			args <- strsplit(meta[["args"]], split=",")[[1]]
			
			fix <- meta[["fix"]]; if( !is.na(fix) ){ aux <- strsplit(fix, split="=")[[1]]; fix <- setNames( aux[2], aux[1] ) }
			
			spl0 <- strsplit( meta[["pred.range4"]], split="," )[[1]]
			##%% check for and set constraints
			# ineqA %*% theta + ineqB < 0 is checked for rejection
			myInf <- 1e100
			constraints <- NULL
			A <- diag(nargs); A[] <- 0; B <- numeric(nargs);
			aa <- 1
			for( a in 1:nargs ){
				spl <- as.numeric(strsplit(spl0[a], split="~")[[1]])
				if( spl[1] != -Inf ){ A[aa, aa] <- 1; B[aa] <- spl[1] }
				aa <- aa+1
			}
			zero <- (rowSums(cbind(A, B)) == 0); A <- A[!zero,]; B <- B[!zero]
			if(	any(A!=0) || any(B!=0)){ A[A==-Inf] <- (-1)*myInf; A[A==Inf] <- myInf; B[B==Inf] <- myInf; constraints <- list(ineqA=rbind(A), ineqB=B)}
			##%%
			
			##%% set start values
			Starts <- setNames( vector("list", nargs), args )
			for( a in 1:nargs ){
				spl <- as.numeric(strsplit(spl0[a], split="~")[[1]])
				if(spl[1] == 0 && spl[2]==1 ){ Starts[[a]] <- 0.5 }
				else{
					if(spl[1] == 0 && spl[2]==Inf ){ Starts[[a]] <- c(x_scale_gamma, x_shape_gamma, x_sd, x_disp_nbinom) }
					else{ Starts[[a]] <- c(x_mean, x_median) }
				}
			}
			
			Starts <- lapply(Starts, function(v){ v[ !is.nan(v) ] })
			
			AllStarts <- cbind(Starts[[1]])
			if( length(Starts)>1 ){
				for( l in 2:length(Starts) ){ 
					v <- rep(Starts[[l]], each=nrow(AllStarts))
					V <- matrix(AllStarts, byrow=TRUE, nrow=length(v), ncol=ncol(AllStarts))
					AllStarts <- cbind(V, v) 
				}
			}
			colnames(AllStarts) <- args
			##%%

			fits <- dofit(X, AllStarts, Package, dst, constraints=constraints)
			rpt <- fit2report(x=X, fits, Package, substring(dst, 2), meta)
			REPORT0[[r0cnt]] <- rpt
			r0cnt <- r0cnt+1
			
		} # END LOOP over distribtions
		
		
	} # END LOOP over packages
	REPORT[["fitting"]] <- REPORT0
	
return(REPORT)
}
