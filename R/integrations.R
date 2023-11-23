source("R/derivatives.R")


#' Rectangle rule
#'
#' Approximates the definite integral of a function using the rectangle rule
#'
#' @param func A string representing a mathematical function
#' @param a The lower limit of integration
#' @param b The upper limit of integration
#' @param n An integer representing the number of subintervals
#'
#' @return A numeric value representing the approximate value of the definite integral using the rectangle rule
#' @export
#'
#' @examples
#' rectangle_rule('log(x)', 1, 2, 1000)
rectangle_rule <- function(func, a, b, n=10){
  h <- (b - a)/n
  I <- f(func, a)
  x <- a
  for (k in 1:(n-1)) {
    x <- x + h
    I <- I + f(func, x)
  }
  return(I*h)
}

#' Trapezoidal rule
#'
#' Approximates the definite integral of a function using the trapezoidal rule
#'
#' @param func A string representing a mathematical function
#' @param a The lower limit of integration
#' @param b The upper limit of integration
#' @param n An integer representing the number of subintervals
#'
#' @return A numeric value representing the approximate value of the definite integral using the trapezoidal rule
#' @export
#'
#' @examples
#' trapezoidal_rule('log(x)', 1, 2, 1000)
trapezoidal_rule <- function(func, a, b, n=10){
  h <- (b - a)/n
  I <- (f(func, a) + f(func, b)) / 2
  x <- a
  for (k in 1:(n-1)) {
    x <- x + h
    I <- I + f(func, x)
  }
  return(I*h)
}
