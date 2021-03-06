% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecdf2.R
\name{ecdf2}
\alias{ecdf2}
\title{Calculate cumulative density}
\usage{
ecdf2(x, y = NULL)
}
\arguments{
\item{x}{A numeric vector of which the ECDF should be calculated}

\item{y}{A numeric vector. See details for explanation}
}
\value{
A list
}
\description{
Calculates the cumulative density of a set of numeric values.
}
\details{
This function extends the functionality of of the standard implementation of ECDF. Sometimes it is desireable to get the ECDF from pre-tabulated values. For this, elements in x and y have to be linked to each other.
}
\examples{
x <- rnorm(1000)
e <- ecdf2(x)
str(e)
plot(e)
plot(e$x, e$cs)

x <- sample(1:100, 1000, replace=TRUE)
plot(ecdf2(x))
tab <- table(x)
x <- unique(x)
lines(ecdf2(x, y=tab), col="green")
}
\seealso{
\code{\link{ecdf}} for the standard implementation of ECDF
}
