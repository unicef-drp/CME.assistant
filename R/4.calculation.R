

#' A rounding function that round off numbers in the conventional way
#'
#' Instead of in R where round(0.5) = 0, roundoff(0.5, 0) = 1
#'
#' @param x the number
#' @param digits digits, default to 2
#' @return rounded numeric vector
#' @export roundoff
roundoff <- function(#
  x, digits = 2
) {
  if(!is.numeric(x)) message("x coerse to numeric. ")
  x <- as.numeric(x)
  z <- trunc(abs(x)*10^digits + 0.5)
  z <- sign(x)*z/10^digits
  return(z)
}


#' Calculate 5q0 from 1q0 and 4q1
#'
#' @param q1 1q0
#' @param q4 4q1
#' @param use_q_not_rate default to TRUE, if TRUE q1, q4 are probability of
#'   dying  dx/lx, if FALSE are mortality rate per 1,000
#'
#' @return 5q0
#' @export
calculate.5q0 <- function(q1, q4, use_q_not_rate = TRUE){
  if(use_q_not_rate){
    if(q1 > 1 | q4 > 1) message("Double check if q1 and q4 are probabilities")
    q <- 1 - (1-q1) * (1-q4)
  } else {
    q <- (1 - (1-q1/1E3) * (1-q4/1E3))*1000
  }
  return(q)
}


#' Calculate 4q1 from 5q0 and 1q0
#'
#' Calculate 4q1 from 5q0 and 1q0 or any part from the whole
#' e.g. NMR from IMR and PNMR

#'
#' @param q5 5q0
#' @param q1 1q0
#' @param use_q_not_rate default to TRUE, if TRUE q5, q1 are probability of
#'   dying  dx/lx, if FALSE are mortality rate per 1,000
#'
#' @return 4q1
#' @export
#'
calculate.4q1 <- function(q5, q1, use_q_not_rate = TRUE){
  if(use_q_not_rate){
    if(q1 > 1 | q5 > 1) message("Double check if q1 and q4 are probabilities")
    q <- 1-(1-q5)/(1-q1)
  } else {
    q <- (1-(1-q5/1000)/(1-q1/1000)) * 1000
  }
  return(q)

}
