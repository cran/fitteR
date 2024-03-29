% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pvalue2stars.R
\name{pvalue2stars}
\alias{pvalue2stars}
\alias{pvalues2stars}
\title{Significance stars}
\usage{
pvalue2stars(x, ns = "")

pvalues2stars(x, ns = "")
}
\arguments{
\item{x}{Numeric value or numeric vector, typically a P-value from a statistical test.}

\item{ns}{A character string specifying how insignificant results should be marked. Empty string by default.}
}
\value{
String(s) of stars or points.
}
\description{
Get stars indicating the magnitude of significance of a P-value.
}
\details{
While the function \code{pvalue2stars} accepts only a single value, the function \code{pvalues2stars} is a wrapper calling \code{pvalue2stars} for a vector.
The range of x is not checked. However, a check is done, if x is numeric at all.
}
\examples{
x <- runif(1, 0,1)
pvalue2stars(x)

x <- 0.5
pvalue2stars(x, ns="not signif")

x <- c(0.0023, 0.5, 0.04)
pvalues2stars(x, ns="not signif")

}
\author{
Markus Boenn
}
