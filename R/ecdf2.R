

#' @title Calculate cumulative density
#'
#' @description
#' Calculates the cumulative density of a set of numeric values.
#' 
#' @param x A numeric vector of which the ECDF should be calculated
#' @param y A numeric vector. See details for explanation
#'
#' @return A list
#'
#' @details
#' This function extends the functionality of of the standard implementation of ECDF. Sometimes it is desireable to get the ECDF from pre-tabulated values. For this, elements in x and y have to be linked to each other.
#' @seealso \code{\link{ecdf}} for the standard implementation of ECDF

#' @examples
#' x <- rnorm(1000)
#' e <- ecdf2(x)
#' str(e)
#' plot(e)
#' plot(e$x, e$cs)
#' 
#' x <- sample(1:100, 1000, replace=TRUE)
#' plot(ecdf2(x))
#' tab <- table(x)
#' x <- unique(x)
#' lines(ecdf2(x, y=tab), col="green")
#' @export



ecdf2 <- function(x, y=NULL){
	x <- sort(x)
	L <- length(x)
	x <- as.matrix(x) # transform x to matrix to keep names after calling 'unique'
	u <- unique(x) # name of first occurence is kept
	vals <- setNames( c(u), rownames(u) )
	yy <- 0
	if( is.null(y) ){
		yy <- cumsum(tabulate(match(x, vals)))/L
	}else{ yy <- cumsum( y )/sum(y) }
	names(yy) <- names(vals)
return( list(x=vals, y=yy, cs=yy*L) )
}

