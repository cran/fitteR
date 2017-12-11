


#' @title Prepare report of fitting
#'
#' @description
#' Prepares a summary of the fitting as csv or shiny
#' 
#' @param x The output of \code{\link{fitter}}
#' @param file A character string giving the filename (including path) where the report should be printed
#' @param type A character vector giving the desired type(s) of output
#'
#' @return A list with items
#' \item{table}{A \code{data.frame} with the same formating as the resulting csv file.}
#' \item{shiny}{if \code{"shiny" \%in\% type}: a shiny object}
#'
#' @details
#' The routine generates a simple csv file, which is the most useful output in terms of reusability. However, the shiny output is more powerful and provides an overview of the statistics and a figure for visual/manual exploration of the fits.
#' Irrspective of output type being \dQuote{csv} or \dQuote{shiny}, the fit-table has the following format
#' \describe{
#' \item{package}{ package name}
#' \item{distr}{ name of the distribution}
#' \item{nargs}{ number of parameters}
#' \item{args}{ names of parameters, comma-seperated list}
#' \item{estimate}{estimated values of parameters, comma-seperated list}
#' \item{start}{start values of parameters, comma-seperated list}
#' \item{constraints}{were constraints used, logical}
#' \item{runtime}{the runtime in milliseconds}
#' \item{KS}{test statistic $D$ of a two-sided, two-sample Kolmogorov-Smirnov test}
#' \item{pKS}{$P$-value of a two-sided, two-sample Kolmogorov-Smirnov test}
#' \item{SW}{test statistic of a Shapiro-Wilks test}
#' \item{pSW}{$P$-value of a Shapiro-Wilks test}
#' }


#' @examples
#' # discrete empirical data
#' x <- rnbinom(100, 0.5, 0.2)
#' r <- fitter(x, dom="dis", posList=list(stats=NA))
#' # create only 'shiny' app
#' out <- printReport(r, type="shiny")
#' names(out)
#' \dontrun{ out$shiny }
#' out <- printReport(r, type=c("csv")) # warning as 'file' is NULL, 
#' str(out) # but table (data.frame) returned
#'
#' @rdname printReport
#' @name printReport
#' @aliases printReport
#' @author Markus Boenn
#' @export









printReport <- function( x, file=NULL, type="csv"  ){
	
	
	rptECDF <- x$ecdf
	
	tmp <- x$emp
	rptEMPEST <- rbind(
			c( "Number of observations", tmp["N"] )
		,	c( "min", tmp["min"] )
		,	c( "max", tmp["max"] )
		,	c( "arith. mean", tmp["mean"] )
		,	c( "median", tmp["median"] )
		,	c( "standard deviation", tmp["sd"] )
		,	c( "scale (gamma)", tmp["scale_gamma"] )
		,	c( "dispersion (neg. binomial)", tmp["dispersion_nbinom"] )
		,	c( "shape (gamma)", tmp["shape_gamma"] )
		,	c( "skew", tmp["skew"] )
	)
	colnames(rptEMPEST) <- c("measure", "value")
	rptEMPEST <- as.data.frame(rptEMPEST)
	rptEMPEST$measure <- as.character(rptEMPEST$measure)
	rptEMPEST$value <- as.numeric(as.character(rptEMPEST$value))
	
	
	.FITTING <- x$fitting
	M <- c()
	for( i in 1:length(.FITTING) ){
		if(is.null(.FITTING[[i]])){ next }
		aux <- unlist( .FITTING[[i]][c("Package", "distr", "nargs", "args")] )
		.FITS <- .FITTING[[i]]$fits
		.GOF <- .FITTING[[i]]$gof
		nargs <- as.integer(aux["nargs"])
		S <- split( 1:(nargs*2), findInterval(1:(nargs*2), seq(1, nargs*2, nargs)) )
		aux2 <- rbind(rep(NA, 2+6+2))
		if( !is.null(.FITS) && nrow( .FITS )>0 ){
			aux2 <- c()
			for( j in 1:nrow(.FITS) ){
				aux2 <- rbind( aux2, c(
					ifelse(is.na(.FITS[j, 1 ]), NA, paste( collapse=",", .FITS[j, S[[1]] ] ))
					, paste( collapse=",", .FITS[j, S[[2]] ] )
					, .FITS[j, tail(S[[2]], 1)+1] # constraints
					, .FITS[j, tail(S[[2]], 1)+2] # runtime
					, .GOF[j,]
				))
			}
		}
		M <- rbind(M, cbind( matrix(aux, nrow=nrow(aux2), ncol=length(aux), byrow=TRUE), aux2 ) )
	}
	M <- as.data.frame(M)
	colnames(M) <- c("Package", "distr", "nargs", "args", "estimate" , "start", "constraints", "runtime", "KS", "pKS", "SW", "pSW", "AD", "pAD")
	numArgs <- c("nargs", "constraints", "runtime", "KS", "pKS", "SW", "pSW", "AD", "pAD")
	for( cn in setdiff(colnames(M), numArgs) ){ M[, cn] <- as.character(M[,cn]) }
	for( cn in numArgs ){ M[, cn] <- as.numeric(as.character(M[,cn])) }
	rptFITS <- M

	myVERSION <- NULL
	if(is.null(myVERSION)){ myVERSION <- packageVersion("fitteR") }
	rptSUMMARY <- rbind(
			c( "Date" , x[["date"]])
		, 	c( "fitteR version", paste0("'fitteR' ", myVERSION, ", by Markus Boenn") )
		, 	c( "Number of distributions", nrow(x[["meta"]]))
		, 	c( "Number of packages", length(unique(x[["meta"]][, "package"])) )
		, 	c( "Domain of empirical data", x[["domain"]] )
		,	c( "Number of candidate distributions", sum(!duplicated(rptFITS[, c("Package", "distr")]) ))
		, 	c( "Number of distributions with fit", sum( !duplicated( rptFITS[!is.na(rptFITS[, "estimate"]), c("Package", "distr")]  ) ) )
	)
	colnames(rptSUMMARY) <- NULL

	
	
	
	
	if( "csv" %in% type ){
		if(is.null(file)){ warning("No csv file is written. Specify 'file'."); }
		else{write.table(file=file, x=rptFITS, sep="\t", col.names=TRUE, row.names=FALSE, quote=FALSE)}
	}


	## split parameters to lists
# 	rptFITS2 <- rptFITS
# 	rptFITS2$args <- lapply(strsplit(rptFITS2$args, ","), as.character)
# 	rptFITS2$estimate <- lapply(strsplit(rptFITS2$estimate, ","), as.numeric)
# 	rptFITS2$start <- lapply(strsplit(rptFITS2$start, ","), as.numeric)

	SHINY <- NULL
	if( "shiny" %in% type ){

		# Define UI
		ui <- fluidPage(
			titlePanel("fitteR results"),
# 			sidebarPanel(
# 				numericInput('roundSummaryData', 'Round', 3, min=0, max=10, step=1)
# 			),
			# Output
			mainPanel(
				fluidRow(
					column(width = 4, h2("Package Summary"), tableOutput("viewSummaryPackage")),
					column(width = 4, h2("Data Summary"), tableOutput("viewSummaryData"))
				)
				
				, hr()
				, h2("Emprical cumulative density function (ECDF)"),
				plotOutput("plotECDF")

				, hr()
				, h2("x"),
				fluidRow(
					column(4, selectInput("pack", "Package:", choices=c("All", unique(as.character(rptFITS$Package))))),
					column(4, selectInput("fit", "Fit:", choices=c("with", "All", "without")), selected="with"),
					column(4, selectInput("roundFits","Rounding:", choices=c(as.character(0:8), "Full"), selected="3")),
					column(4, selectInput("dupl","Duplicates:", choices=c("drop", "keep"), selected="drop"))
				),
				# Create a new row for the table.
				fluidRow(
					DT::dataTableOutput("viewx")
				)
				
				# ecdf of selected rows
				,plotOutput("plotFits")
			) # END mainPanel
		)

		# Define server function
		server <- function(input, output){
			output$viewSummaryPackage <- renderTable({
				rptSUMMARY
			}, colnames=FALSE)

			
			roundedEMPEST <- reactive( { aux <- rptEMPEST; aux$value <- round(aux$value, 5); aux })
			output$viewSummaryData <- renderTable({
				roundedEMPEST()
			}, colnames=FALSE)

			output$plotECDF <- renderPlot({
				plot(rptECDF, xlab="x", ylab="P(X<x)", type="b")
				legend("bottomright", col="black", lwd=2, legend="emp")
			})
			

			my_filteredTableViewx <- function(tmp){
				Package <- NULL
				if( input$pack != "All" ){ tmp <- rbind(dplyr::filter(tmp, Package==input$pack)) }
				if( input$fit != "All" ){ 
					if(input$fit == "with"){ tmp <- tmp[ !is.na(tmp$estimate), , drop=FALSE] }
					if(input$fit == "without"){ tmp <- tmp[ is.na(tmp$estimate), , drop=FALSE] }
				}
				if( input$roundFits != "Full" ){
					for( cn in c("estimate", "start", "KS", "SW", "AD", "pKS", "pSW", "pAD") ){
						isNA <- is.na(tmp[, cn])
						tmp[!isNA, cn] <- vapply( strsplit(as.character(tmp[!isNA, cn]), ","), function(v){ 
							paste(collapse=",", round(as.numeric(v), as.numeric(input$roundFits))) 
						}, FUN.VALUE="abc"); 
					}
				}

				if( input$dupl != "keep" ){
					cn <- "estimate"
					TMP <- split(tmp, paste(sep=":", tmp[, "Package"], tmp[, "distr"]))
					TMP <- lapply(TMP, function(v){v[!duplicated(v[, cn]),]})
					tmp <- do.call("rbind", TMP)
					rownames(tmp) <- NULL
				}
				tmp
			}
			filteredTableViewx <- reactive({
				tmp <- rptFITS
				tmp <- my_filteredTableViewx(tmp)
				tmp
			})

			output$viewx <- DT::renderDataTable({ 
				DT::datatable({
					tmp <- filteredTableViewx()
					

				
					##%% create link to package website
					port <- tools::startDynamicHelp(NA)

					
					Pack <- tmp$Package
					tmp$Package <- vapply( tmp$Package, function(v){ if(v=="stats"){return("stats")}; paste0('<a href="https://CRAN.R-project.org/package=', v, '" target="_blank" class="btn btn-primary">', v, '</a>') }, FUN.VALUE="abc" )
					
					
					
					
					
					tmp$distr <- vapply( 1:length(tmp$distr), function(k){ v <- tmp$distr[k]; paste0( '<a href="http://127.0.0.1:', port, '/library/', as.character(Pack[k]), '/html/d', v, '.html" target="_blank" class="btn btn-primary">', v, '</a>' ) }, FUN.VALUE="abc" )
					
					tmp
				}, escape=FALSE)
			})
			#C, options = list(autoWidth = TRUE))
			
			
			filteredTableViewxSel <- reactive({
				sel <- input$viewx_rows_selected
				if(is.null(sel)){return(NULL)}
				filteredTableViewx()[sel, ]
			})

			
			output$plotFits <- renderPlot({
				TMP <- filteredTableViewxSel()
				if(!is.null(TMP) && nrow(TMP)){
					Col <- heat.colors(nrow(TMP))
					Estimate <- strsplit(as.character(TMP[, "estimate"]), ",")
					Args <- strsplit(as.character(TMP[, "args"]), ",")
					plot(rptECDF, xlab="x", ylab="P(X<x)", type="p")

					for( s in 1:nrow(TMP) ){
						tmp <- TMP[s, ]
						if(is.na(tmp["estimate"])){next}
						foo <- get(paste0("p", as.character(tmp["distr"])), asNamespace(as.character(tmp["Package"])))
						PARAM <- c( list(rptECDF$x), as.list(setNames( as.numeric(Estimate[[s]]), Args[[s]] ) ))

						lines( x=rptECDF$x, y=do.call(foo, PARAM), col=Col[s] )
					}
					legend( "bottomright", col=Col, lwd=2, legend=paste(sep=":", TMP[, "Package"], TMP[, "distr"]) )
				}
			})
		}

		# Create Shiny object
		SHINY <- shinyApp(ui = ui, server = server)
	}
	
invisible( list( table=rptFITS, shiny=SHINY  ) )
} 


