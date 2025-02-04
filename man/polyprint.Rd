% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polynomials.R
\name{polyprint}
\alias{polyprint}
\title{Print polynomials}
\usage{
polyprint(pol)
}
\arguments{
\item{pol}{A coefficient vector representing the polynomial}
}
\value{
A string representation of the polynomial represented by \code{pol}
}
\description{
Returns a string representation of the polynomial
}
\examples{
polyprint(c(-1, 0, 1, 0)) #output: "-x^3 + x"
polyprint(c(2, 0, 1)) #output: "2 * x^3 + x"
}
