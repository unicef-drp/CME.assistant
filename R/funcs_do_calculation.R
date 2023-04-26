# functions to do calculations

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
    if(q1[1] > 1 | q4[1] > 1) message("Double check if q1 and q4 are probabilities (< 1) or mortality rate")
    q <- 1 - (1 - q1) * (1 - q4)
  } else {
    q <- (1 - (1 - q1 / 1E3) * (1 - q4 / 1E3)) * 1E3
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
    if(q1[1] > 1 | q5[1] > 1) message("Double check if q1 and q4 are probabilities (< 1) or mortality rate")
    q <- 1 - (1 - q5) / (1 - q1)
  } else {
    q <- (1 - (1 - q5 / 1E3) / (1 - q1 / 1E3)) * 1E3
  }
  return(q)
}


#' Get the logarithmic annual rate of reduction, expressed as a percentage
#' reduction
#'
#' The formula for ARR is as follows: ARR = ln(rate2/rate1)/(t2-t1)*-100, where
#' t1 and t2 are years and t1<t2. Return unrounded results
#'
#'
#' @param dt wide data by year, year1 and year2 shall be columns like 1990, 2000
#' @param year1 year1 where year1 < year2
#' @param year2 year2 where year1 < year2
#' @return dt with added columns named like "1990-2000"
#' @export calculate.arr
#' @examples
#' \dontrun{
#' dt_final_ARR <- dt_final[Year%in%c(1990,2019)]
#' dt_final_ARR_w <- dcast(dt_final_ARR, ISO3Code ~ Year)
#' dt_final_ARR_w <- calculate.arr(dt_final_ARR_w, 1990, 2019)
#' }
#'
calculate.arr <- function(dt, year1, year2){
  get.year <- function(years){
    if(grepl("\\.5", years)) years <- gsub("\\.5", "", years)
    as.numeric(gsub("[^\\d]+", "", years, perl = TRUE))
  }
  # use year1 and year2 value to get ARR
  dt[, arr:= log(get(as.character(year2))/get(as.character(year1)))/(get.year(year2)-get.year(year1))*-100]
  setnames(dt, "arr", paste0(year1, "-", year2))
  return(dt)
}

#' Calculate percentage decline
#'
#' Calculate percentage decline and return unrounded results
#' @inheritParams calculate.arr
#' @return dt with added columns named like "PD1990-2000"
#' @export calculate.pd
#'
calculate.pd <- function(dt, year1, year2){
  get.year <- function(years){
    if(grepl("\\.5", years)) years <- gsub("\\.5", "", years)
    as.numeric(gsub("[^\\d]+", "", years, perl = TRUE))
  }  # use year1 and year2 value to get ARR
  dt[, pd:= (get(as.character(year2))/get(as.character(year1)) - 1)*-100]
  setnames(dt, "pd", paste0("PD", year1, "-", year2))
  dt
}



#' A rounding function that rounds off numbers in the conventional way: rounds 0.5 to 1
#'
#' Instead of in R by default round(0.5) = 0, roundoff(0.5, 0) = 1
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
