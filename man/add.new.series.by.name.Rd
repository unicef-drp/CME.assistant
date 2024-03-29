% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs_add_new_data_series.R
\name{add.new.series.by.name}
\alias{add.new.series.by.name}
\title{Add new series, match by both `IGME_Key` and `Series.Name`}
\usage{
add.new.series.by.name(
  dt_master,
  dt_new_entries,
  old_entries_action = "no_change"
)
}
\arguments{
\item{dt_master}{master dataset}

\item{dt_new_entries}{new entries}

\item{old_entries_action}{default to "hide", set old series (matched by both
`IGME_Key` and `Series.Name`) to invisible = 0 and inclusion = 0; if
"remove", remove old series; if "no_change": leave as it is}
}
\description{
This is a very strict match
}
