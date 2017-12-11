
#' @title Significance stars
#' @description
#' Get stars indicating the magnitude of significance of a P-value.
#' 
#' @param x Numeric value or numeric vector, typically a P-value from a statistical test.
#' @param ns A character string specifying how insignificant results should be marked. Empty string by default.
#' 
#' 
#' @details
#' While the function \code{pvalue2stars} accepts only a single value, the function \code{pvalues2stars} is a wrapper calling \code{pvalue2stars} for a vector.
#' The range of x is not checked. However, a check is done, if x is numeric at all.
#' 
#' @return String(s) of stars or points.
#' 
#' 
#' @examples
#' x <- runif(1, 0,1)
#' pvalue2stars(x)
#' 
#' x <- 0.5
#' pvalue2stars(x, ns="not signif")
#' 
#' x <- c(0.0023, 0.5, 0.04)
#' pvalues2stars(x, ns="not signif")
#'
#' @name pvalue2stars
#' @rdname pvalue2stars
#' @author Markus Boenn
#' @aliases pvalue2stars pvalues2stars
#' @export



pvalue2stars <- function( x, ns="" ){
	if( .Not_A_Number(x) ){ return("") }
	if( x>0.1 ){ return(ns) }
	ifelse( x < 0.0001, '****', ifelse( x < 0.001, '***', ifelse( x < 0.01, '**', ifelse( x < 0.05, '*', ifelse( x < 0.1, '.', "" ) ) ) ) )
}

#' @rdname pvalue2stars
pvalues2stars <- function( x, ns="" ){
sapply( x, pvalue2stars, ns=ns )
}
 

.Not_A_Number <- function(x){ is.na(x) || is.nan(x) || x==Inf || x==-Inf }
