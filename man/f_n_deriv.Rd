% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derivatives.R
\name{f_n_deriv}
\alias{f_n_deriv}
\title{Calculates the nth derivative of a function at a given point}
\usage{
f_n_deriv(func, n = 0, a = 0)
}
\arguments{
\item{func}{A string representing a mathematical function}

\item{n}{An integer specifying the order of the derivative}

\item{a}{A numeric value representing the point at which to calculate the derivative}
}
\value{
A numeric value representing the nth derivative of the function at the given point
}
\description{
The function calculates the nth derivative of a given function
at a specified point. It considers a function to be derived and
uses the binomial coefficient and the 'f' function to calculate
the derivative
}
\examples{
f_n_deriv('exp(x)', 2, 1) #Returns the second derivative of exp(x) at x = 1
}
