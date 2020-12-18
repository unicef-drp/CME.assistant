% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3.data_subset_dataset.R
\name{add.new.series.u5mr}
\alias{add.new.series.u5mr}
\title{Add new U5MR or IMR entries (dt_new_entries)}
\usage{
add.new.series.u5mr(dt_master, dt_new_entries, remove_old = TRUE)
}
\arguments{
\item{dt_master}{the U5MR master dataset}

\item{dt_new_entries}{dt of new entries to be added}

\item{remove_old}{if TRUE, remove old entries, if FALSE, set old entries to invisible and excluded}
}
\value{
the new dt_master
}
\description{
Add new U5MR or IMR entries (dt_new_entries)
}