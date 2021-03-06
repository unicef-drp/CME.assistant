% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3.data_subset_dataset.R
\name{add.new.VR}
\alias{add.new.VR}
\title{Add new VR series}
\usage{
add.new.VR(dt_master, dt_new_entries, NMR = FALSE, hide_old = TRUE)
}
\arguments{
\item{dt_master}{master dataset}

\item{dt_new_entries}{new entries}

\item{NMR}{is it NMR or not, only controls how the columns are sorted}

\item{hide_old}{default to TRUE, set old ones to invisible = 0 and inclusion
= 0, otherwise leave as it is}
}
\description{
Add new VR series
}
