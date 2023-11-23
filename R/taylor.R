source("R/derivatives.R")
source("R/polynomials.R")

#' Taylor
#'
#' Approximates a function using Taylor series expansion
#' @param func A string representing a mathematical function or a polynomial
#' @param x The value at which to evaluate the Taylor series
#' @param n An integer specifying the degree of the Taylor series
#' @param a The point at which to calculate the Taylor series
#' @param pol A logical value indicating whether  to return the Taylor polynomial
#'
#' @return if \code{pol} == FALSE, a numeric value representing the approximate value of the function
#' @return if \code{pol} == TRUE, a polynomial object representing the Taylor polynomial
#' @export
#'
#' @examples
#' taylor('sin(x)', x = 1, n = 3, a = 0) #0.8283376
taylor <- function(func, x=0, n = 0, a = 0, pol=FALSE){
  k <-  0
  taylor <-  0
  polynomial <- c(0)
  while (f_n_deriv(func, n=k, a=a) != Inf & f_n_deriv(func, n=k, a=a) != -Inf & k<=n)
  {
    if(!pol){
      taylor <-  taylor +  (f_n_deriv(func, n=k, a=a) * ((x-a)^k))/factorial(k)
    }

    else{
      polynomial <- polyadd(polynomial, f_n_deriv(func, n=k, a=a) * (polypow(c(1, -a),k))/factorial(k))
    }

    k <- k+1
  }

  if(!pol){
    return(taylor)
  }

  else{
    return(polynomial)
  }
}
