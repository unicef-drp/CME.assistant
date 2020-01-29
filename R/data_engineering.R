# Data engineering related helper functions
# CME functions package
# more to be added
# Yang Liu

# Extra -------------------------------------------------------------------

#' check and install pkgs if missing
#' @importFrom utils install.packages installed.packages
#' @param pkgs vector of packages
#' @return NULL
#' @export check.and.install.pkgs
#'
check.and.install.pkgs <- function(pkgs){
  new.packages <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
}

#' a round function that Round off numbers in the conventional way instead of the R round
#' In R, round(0.5) = 0
#'
#' @param x the number
#' @param digits digits, default to 2
#'
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
get.match <- function(x,
                      new_list = NULL,
                      no_line_break = FALSE){
  if(is.null(new_list)){
    labs <- default_label_1
  } else {
    if(is.list(new_list)){
      labs <- new_list
    } else {
      message("new_list must be a list. Still use the default list.")
      labs <- default_label_1
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

#' Read in IGME 2019 estimates, using CME_2019_all_data
#' 01/24/2020
#' @importFrom here here
#'
#' @param ind choose indicators among U5MR, NMR, and IMR
#' @param get both, rate, or death
#' @param c_name country by official name
#' @param c_iso get country by ISO, will overwrite c_name if provided
#' @param year_range a vector, e.g. 1990:2018
#'
#' @return a data.table
#' @export get.CME.estimates.long
#' @examples dt_cme_long <- get.CME.estimates.long(ind = "U5MR", c_name = "Mozambique", year_range = 2016:2018)
get.CME.estimates.long <- function(ind = c("U5MR", "NMR"),
                                   get = "both",
                                   c_iso = NULL,
                                   c_name = "Mozambique",
                                   year_range = 1990:2018
){
  if(file.exists(here::here("data", "CME_2019_all_data.csv"))) {
    dt1 <- fread(here::here("data", "CME_2019_all_data.csv"))
  } else {
    dt1 <- CME.org_2019_all_data
  }
  if(!all(ind%in%c("U5MR", "NMR", "IMR"))) stop("choose indicators among U5MR, NMR, and IMR")
  match_ind <- list("U5MR" = "Under-five mortality rate",
                    "NMR"  = "Neonatal mortality rate",
                    "IMR"  = "Infant mortality rate",
                    "U5MR_death" = "Under-five deaths",
                    "NMR_death"  = "Neonatal deaths",
                    "IMR"  = "Infant deaths")
  if(get == "both"){
    inds_wanted <- c(get.match(ind, new_list = match_ind), get.match(paste0(ind, "_death"), new_list = match_ind))
  } else if (get == "rate") {
    inds_wanted <- get.match(ind, new_list = match_ind)
  } else if (get == "death") {
    inds_wanted <- get.match(paste0(ind, "_death"), new_list = match_ind)
  } else {
    stop("get either rate, death, or both")
  }
  vars_wanted <- c("REF_AREA", "INDICATOR", "TIME_PERIOD", "UNIT_MEASURE",
                   "OBS_VALUE", "LOWER_BOUND", "UPPER_BOUND")
  if(!is.null(c_iso)) c_name <- dt1[ISO3Code == c_iso, REF_AREA][1]
  dt2 <- dt1[INDICATOR%in%inds_wanted][REF_AREA == c_name][SEX=="Total"][SERIES_NAME=="UN IGME estimate 2019"][,..vars_wanted]
  dt2[, year:= as.numeric(substr(TIME_PERIOD, 1, 4))]
  dt2 <- dt2[year%in%year_range][, TIME_PERIOD:= NULL]
  setnames(dt2, c("Country", "Indicator", "Unit","Estimate", "Lower_Bound", "Upper_Bound", "Year"))
  dt_cme_long <- dt2[,c("Country", "Year", "Indicator", "Unit", "Estimate", "Lower_Bound", "Upper_Bound"), with = FALSE]
  cols <- c("Estimate", "Lower_Bound", "Upper_Bound")
  dt_cme_long[, c(cols):=lapply(.SD, roundoff, digits = 2), .SDcols = cols]
  setorder(dt_cme_long, -Indicator, -Year)
  return(dt_cme_long)
}

#' Get wide format for dataset
#'
#' @param dt_long a long-format data with "Country", "Year", "Indicator","Estimate", "Lower_Bound", "Upper_Bound"
#' @return  a wide-format data
#' @export get.dt.wide
#' @examples dt_cme_wide <- get.dt.wide(get.CME.estimates.long(c_name = "China", year_range = 2018))
get.dt.wide <- function(dt_long){
  dt_wide <- dcast.data.table(dt_long, Country  + Year  ~ Indicator, value.var = c("Estimate", "Lower_Bound", "Upper_Bound"))
  # remove the "Estimate_" in variable names
  setnames(dt_wide, sub("Estimate_", "", colnames(dt_wide)))
  # set new order, which put each indicator together
  new_col_order <- c("Country", "Year",
                     do.call(paste, c(expand.grid(c("", "_Lower_Bound", "_Upper_Bound"),
                                                  unique(dt_long$Indicator)), sep = "_")))
  # remove the "_" in front of string
  dt_wide <- dt_wide[, sub(".*?_", "" ,new_col_order), with = FALSE]
  setorder(dt_wide, -Year)
  return(dt_wide)
}

#' old and simpler version: get three types (Under-five Infant Neonatal) from
#' Rates & Deaths summary using the wide format data source, without SE 2009/10
#' @import data.table
#' @importFrom readr parse_number
#'
#' @param year_range a vector of years we want, default to 2000:2018
#' @param dir_file dir to the file to read
#' @param get_what "Deaths" or "Rate", default to "Rate": get the three CME rate
#'
#' @examples
#' get.CME.data(year_range = c(2016:2018))
#' @export get.CME.data
#' @return dt of ISO3, UNcode, year, Under-five, Infant, Neonatal, one row for
#'   each country each year in year_range
#'
get.CME.data <- function(year_range = c(2019:2030), get_what = "rate",
                             dir_file = dir_summary_file){
  dt <- fread(dir_file)
  available_years <- readr::parse_number(grep("IMR", names(dt), value = TRUE))
  if (!all(year_range%in%available_years)) {
    stop("Available years are between: ", paste(range(available_years), collapse = " and "),
         ". Set years to default range.")
  }

  if(get_what == "rate") {
    CME_types_full <- c("U5MR", "IMR", "NMR")
    vars_wanted <- c("ISO3Code",	"UNCode", "OfficialName",
                     do.call(paste, expand.grid(CME_types_full, year_range)))
  } else if (get_what == "death") {
    CME_types_full <- c("Under-five Deaths", "Infant Deaths", "Neonatal Deaths")
    # get all the combination for variable names: e.g. Under-five Deaths 2000, 19*3 = 57 variables
    vars_wanted <- c("ISO3Code",	"UNCode", "OfficialName",
                     do.call(paste, expand.grid(CME_types_full, year_range)))
  } else (
    stop("choose get_what between rate and death.")
  )

  if(nrow(dt) == dt[,uniqueN(ISO3Code)] * 3){
    message("Extract Upper and Lower bound instead")
    setnames(dt, "V77", "UI", skip_absent = TRUE)
    vars_wanted <- c("ISO3Code",	"UNCode", "OfficialName", "UI",
                     do.call(paste, expand.grid(CME_types_full, year_range)))
  }

  # melt by CME_types_full using `patterns`
  dt_death_long <- melt.data.table(dt[,..vars_wanted],
                                   measure = patterns(paste0("^", CME_types_full)),
                                   value.name = CME_types_full, variable.name = "Year")
  levels(dt_death_long$Year) <- year_range
  dt_death_long[, Year:=as.numeric(levels(Year)[Year])]
  dt_death_long[order(ISO3Code)]

  if(nrow(dt) == dt[,uniqueN(ISO3Code)] * 3) dt_death_long <- dt_death_long[UI%in%c("Lower", "Upper"),]
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


