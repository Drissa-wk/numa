
#' Print polynomials
#'
#' Returns a string representation of the polynomial
#' @param pol A coefficient vector representing the polynomial
#'
#' @return A string representation of the polynomial represented by \code{pol}
#' @export
#'
#' @examples
#' polyprint(c(-1, 0, 1, 0)) #output: "-x^3 + x"
#' polyprint(c(2, 0, 1)) #output: "2 * x^3 + x"
polyprint <- function(pol){
  if(length(pol) == 0){
    return("Le polynome n'est pas defini")
  }
  p <- round(pol, digits = 5)  #round to five decimal places
  lp <- length(p)
  polynomial <- ''

  if(length(p) == 1){
    return(p)
  }

  for(i in 1:lp){
    if(p[i] < 0 & p[i] != -1){
      polynomial <- paste(polynomial, ' - ', -p[i], ifelse(i!=lp,paste('* x',
                                                                       ifelse(i!=lp-1, paste('^', lp-i), '')), ''))
    }

    if(p[i] == -1){
      polynomial <- paste(polynomial, ' - ', ifelse(i!=lp,paste('x',
                                                                ifelse(i!=lp-1, paste('^', lp-i), '')), -p[i]))
    }

    if(p[i] == 0){
      polynomial <- polynomial
    }

    if(p[i] > 0 & p[i] != 1){
      polynomial <- paste(polynomial, ifelse(i==1, '', ' + '), p[i], ifelse(i!=lp,paste('* x',
                                                                                        ifelse(i!=lp-1, paste('^', lp-i), '')), ''))
    }

    if(p[i] == 1){
      polynomial <- paste(polynomial, ifelse(i==1, '', ' + '), ifelse(i!=lp,paste('x',
                                                                                  ifelse(i!=lp-1, paste('^', lp-i), '')), p[i]))
    }

  }
  return(polynomial)
}




#' Addition of polynomials
#'
#' Performs the addition of two polynomials represented by coefficient vectors
#' @param p A coefficient vector representing the first polynomial
#' @param q A coefficient vector representing the second polynomial
#'
#' @return A coefficient vector representing the resulting polynomial from the addition
#' @export
#'
#' @examples
#' polyadd(c(1, 0, -1), c(1, 0, 3, 5)) #output: c(1, 1, 3, 4)
#' polyprint(polyadd(c(1, 2, 3), c(4, 5, 6)))
polyadd <- function(p, q)
{
  lp <- length(p)
  lq <- length(q)
  if(lp > lq){
    q <- c(rep(0, times=lp-lq), q)
  }

  if(lp < lq){
    p <- c(rep(0, times=lq-lp), p)
  }

  return(p+q)
}



#' Multiplication of two polynomials
#'
#' performs the multiplication of two polynomials represented by coefficient vectors
#' @param p A coefficient vector representing the first polynomial
#' @param q A coefficient vector representing the second polynomial
#'
#' @return A coefficient vector representing the resulting polynomial from the multiplication
#' @export
#'
#' @examples
#' polymul(c(1, 0, -1), c(3, 0, 1, 5))
#' polyprint(polymul(c(1, 3), c(4, 5)))
polymul <- function(p, q)
{
  lp <- length(p)
  lq <- length(q)
  mul<- c(rep(0, lp+lq))
  for(i in 1:lp)
  {
    for(j in 1:lq)
    {
      index <- i + j
      mul[index] <- mul[index] + p[i] * q[j]
    }
  }

  return(mul[2:(lp+lq)])
}



#' Raises a polynomial to a given power
#'
#' @param p  A coefficient vector representing the first polynomial
#' @param n   A coefficient vector representing the first polynomial
#'
#' @return A coefficient vector representing the resulting polynomial from the pow
#' @export
#'
#' @examples
#' polypow(c(1, 0, -1), 3)
#' polyprint(polypow(c(1, 1), 2))
polypow <- function(p, n=1){
  if(n==0){
    return(1)
  }
  pow <- 1
  for(i in 1:n)
  {
    pow <- polymul(pow, p)
  }
  return(pow)
}


