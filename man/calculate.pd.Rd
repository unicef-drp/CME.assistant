% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs_read_data.R
\name{calculate.pd}
\alias{calculate.pd}
\title{Calculate percentage decline}
\usage{
calculate.pd(dt, year1, year2)
}
\arguments{
\item{dt}{wide data by year, year1 and year2 shall be columns like 1990, 2000}

\item{year1}{year1 where year1 < year2}

\item{year2}{year2 where year1 < year2}
}
\value{
dt with added columns named like "PD1990-2000"
}
\description{
Calculate percentage decline and return unrounded results
}
