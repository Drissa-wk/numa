source("R/derivatives.R")


#' Newton-Raphson
#'
#' Approximate the root of a function using the Newton-Raphson method
#'
#' @param func A string representing a mathematical function
#' @param x0 The initial guess for the root
#' @param erreur The desired level of accuracy
#'
#' @return A numeric value representing the approximate root of the function using the Newton-Raphson method
#' @export
#'
#' @examples
#' newton_raphson('x^2 - 3*x - 4', 6) #4.000001
newton_raphson <- function(func, x0, erreur=0.00001){
  dfx0 <-  f_n_deriv(func=func, n = 1, a = x0)
  fx0 <-  f(func=func, x=x0)
  while(dfx0 != -Inf && dfx0 != 0 && dfx0 != Inf && fx0 > erreur){
    x0 <- x0 - fx0/dfx0
    dfx0 <-  f_n_deriv(func=func, n = 1, a = x0)
    fx0 <-  f(func=func, x=x0)
  }
  return(x0)
}
