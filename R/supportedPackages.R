




#' @title Supported packages
#'
#' @description
#' Get a list of currently supported packages
#' 
#' @return A character vector
#'
#' @details
#' Numerous R-packages are supported, each providing a couple of theoretical statistical distributions for discrete or continuous data. Beside ordinary distributions like normal, t, exponential, ..., some packages implement more exotic distributions like truncrated alpha.
#'
#' @note
#' Some of the distributions are redundant, i.e. they are implemented in more than one package.
#' 
#' @examples
#' sp <- supported.packages()
#' head(sp)
#' 
#'
#' @aliases supported.packages
#' @name supported.packages
#' @rdname supported.packages
#' @author Markus Boenn
#' @export
 

supported.packages <- function(){
return(as.character( unique( META1[, "package"] ) ))
}
