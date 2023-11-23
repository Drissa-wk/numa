#' Combinaison
#'
#' This function calculates the binomial coefficient "k choose n",
#' which represents the number of ways to choose k elements from a
#' total of n elements
#' @param k An integer representing the number of elements to choose
#' @param n An integer representing the total number of elements
#'
#' @return A numeric value corresponding to the binomial coefficient "k choose n"
#' @export
#'
#' @examples
#' combinaison(2, 5) #returns 10
combinaison <- function(k, n)
{
  if(k <= n){
    return(factorial(n) / (factorial(k) * factorial(n-k)))
  }
  else{
    return(0)
  }

}



#' Evaluate a mathematical function at a given value
#'
#'  The function takes a mathematical function represented as a string
#'  and evaluates it at a specified value. It replaces all the occurences
#'  of 'x' and calculates the value of the function
#' @param func  A string representing a mathematical function
#' @param x The value at which to evaluate the function
#'
#' @return A numeric value representing the result of evaluating the function at the given value
#' @export
#'
#' @examples
#' f("exp(x)", 5)  #Returns the value of exp(5)
f <- function(func, x)
{
  #If you use gsub('x', 5, 'exp(x)'), it will replace all the ‘x’ in the string, including the one in ‘exp’.
  #So, ‘exp(x)’ would become ‘e5p(5)’, which is not what you want.
  #On the other hand, if you use `gsub(‘\bx\b’, 5, ‘exp(x)’), it will only replace the ‘x’ that
  #is a whole word (i.e., it is not preceded or followed by another alphabetical character).
  #So, ‘exp(x)’ will become ‘exp(5)’

  exp <- gsub('\\bx\\b', x, func)  #remplace tous les x dans l'expression par la valeur de x
  return(eval(parse(text = exp)))   #evalue l'expression et calcule la valeur de la fonction
}




#' Calculates the nth derivative of a function at a given point
#'
#' The function calculates the nth derivative of a given function
#' at a specified point. It considers a function to be derived and
#' uses the binomial coefficient and the 'f' function to calculate
#' the derivative
#'
#' @param func A string representing a mathematical function
#' @param n An integer specifying the order of the derivative
#' @param a A numeric value representing the point at which to calculate the derivative
#'
#' @return A numeric value representing the nth derivative of the function at the given point
#' @export
#'
#' @examples
#' f_n_deriv('exp(x)', 2, 1) #Returns the second derivative of exp(x) at x = 1
f_n_deriv <- function(func, n = 0, a = 0)
{
  #derivee a l'ordre n au point a
  #fonction f à deriver

  f <- function(x)
  {
    #If you use gsub('x', 5, 'exp(x)'), it will replace all the ‘x’ in the string, including the one in ‘exp’.
    #So, ‘exp(x)’ would become ‘e5p(5)’, which is not what you want.
    #On the other hand, if you use `gsub(‘\bx\b’, 5, ‘exp(x)’), it will only replace the ‘x’ that
    #is a whole word (i.e., it is not preceded or followed by another alphabetical character).
    #So, ‘exp(x)’ will become ‘exp(5)’

    exp <- gsub('\\bx\\b', x, func)  #remplace tous les x dans l'expression par la valeur de x
    return(eval(parse(text = exp)))   #evalue l'expression et calcule la valeur de la fonction
  }

  #calcul de la derivee nieme en a
  derv <-  0
  h <-  0.01
  for(i in 0:n){
    derv <-  derv + ((-1)^i * combinaison(i, n) * f(a + (n-i)*h)) / h^n  #derivee d'ordre n
  }

  return(derv)

}
