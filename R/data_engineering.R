# Data engineering related helper functions
# CME functions package
# more to be added
# Yang Liu


# Extra -------------------------------------------------------------------

#' a round function that Round off numbers in the conventional way instead of the R round
#' In R, round(0.5) = 0
#'
#' @param x the number
#' @param digits digits, default to 2
roundoff <- function(#
  x, digits = 2
) {
  if(!is.numeric(x)) message("x coerse to numeric. ")
  x <- as.numeric(x)
  z <- trunc(abs(x)*10^digits + 0.5)
  z <- sign(x)*z/10^digits
  return(z)
}


#' a label function to relabel certain variable names
#'
#' You can provide a __new_list__ to define the labels. If any label is not
#' provided, the function will just return the original value
#'
#' @param x a element or a vector
#' @param new_list if you supply a new list the function will use instead of the
#'   default_labels
#' @param no_line_break to remove linebreak from the string
#' @export get.match
#' @return updated labels as character vector
get.match <- function(x, new_list = NULL, no_line_break = FALSE){
  default_labels <- default_label_1
  if(is.null(new_list)){
    labs <- default_labels
  } else {
    if(is.list(new_list)){
      labs <- new_list
    } else {
      message("new_list must be a list. Still use the default list.")
      labs <- default_labels
    }
  }
  if(!is.character(x)){
    message("Coerse input into character.")
    x <- as.character(x)
  }
  out <- rep(NA, length(x))
  for (i in 1:length(x)){
    if (is.null(labs[[ x[i] ]])){
      out[i] <- x[i]
    }else{
      out[i] <- labs[[ x[i] ]]
    }
  }
  return(if(no_line_break)gsub("\n", "", out) else out)
}


# Read data ---------------------------------------------------------------


#' get three types (Under-five Infant Neonatal) from Rates & Deaths summary
#' 2009/10
#' @import data.table
#' @importFrom readr parse_number
#' @param year_range a vector of years we want, default to 2000:2018
#' @param get_what "Deaths" or "Rate", default to "Rate": get the three CME rate
#' @examples
#' get.CME.data(year_range = c(2016:2018))
#' @export get.CME.data
#' @return dt of ISO3, UNcode, year, Under-five, Infant, Neonatal, one row for each country each year in year_range
#'
get.CME.data <- function(year_range = c(2000:2018), get_what = NULL){
  dt <- Rates_Deaths_Country_Summary_2019
  available_years <- readr::parse_number(grep("IMR", names(dt), value = TRUE))
  if (!all(year_range%in%available_years)) {
    warning("Available years are between: ", paste(range(available_years), collapse = " and "),
            ". Set years to default range.")
    year_range <- c(2000:2018) # set to default range
  }

  if(is.null(get_what)) {
    CME_types_full <- c("U5MR", "IMR", "NMR")
    vars_wanted <- c("ISO3Code",	"UNCode", "OfficialName",
                     do.call(paste, expand.grid(CME_types_full, year_range)))
  } else {
    CME_types_full <- c("Under-five Deaths", "Infant Deaths", "Neonatal Deaths")
    # get all the combination for variable names: e.g. Under-five Deaths 2000, 19*3 = 57 variables
    vars_wanted <- c("ISO3Code",	"UNCode", "OfficialName",
                     do.call(paste, expand.grid(CME_types_full, year_range)))
  }
  # melt by CME_types_full using `patterns`
  dt_death_long <- data.table::melt(dt[,..vars_wanted],
                                    measure = patterns(paste0("^", CME_types_full)),
                                    value.name = CME_types_full, variable.name = "year")
  levels(dt_death_long$year) <- year_range
  dt_death_long[, year:=as.numeric(levels(year)[year])]
  dt_death_long[order(ISO3Code)]
  return(dt_death_long)
}

#' load the country info file
#' @import data.table
#' @export get.country.info
#' @return dataset of country info
get.country.info <- function(){
  dt <- country_info
  # UNICEFReportRegion2 offers subregions for ECA and SSA, combined into UNICEFReportRegion
  dt[, UNICEFReportRegion:=UNICEFReportRegion1]
  dt[UNICEFReportRegion2!="", UNICEFReportRegion:=UNICEFReportRegion2]
  return(dt)
}

