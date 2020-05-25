# Data engineering related helper functions
# CME functions package
# more to be added
# Yang Liu

# Extra -------------------------------------------------------------------

#' search all the folders in `target.dir`, and list files containing the `file_name_string`
#'
#' @param target.dir target directory
#' @param file_name_string e.g. "data_U5MR"
#' @param full_path full path or not
search.for.file <- function(target.dir, file_name_string, full_path = FALSE){
  n <- which(grepl(file_name_string, list.files(target.dir, recursive = TRUE)))
  list.files(target.dir, recursive = TRUE, full.names = full_path)[n]
}

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


# Show data ---------------------------------------------------------------


#' return different format of data using the final aggregate results country
#' summary
#'
#' @param c_iso country iso, if NULL, returns all isos, default to NULL
#' @param year_range a vector of years, default to 1990: 2018
#' @param ind be either "U5MR", "NMR", "IMR", "all", default to "all"
#' @param idvars default to "OfficialName, ISO3Code", what id vars you want to
#'   include
#' @param dir_file allow a different dataset to be read
#' @param match_ind a different rules to create variable to read using
#'   `get.match`
#' @param format Choose format among raw, long, wide_year, wide_ind, and
#'   wide_get, default to "long". All the wide-format just decasts the
#'   long-format data
#' @param get  get either "rate", "death", or "both", default to "rate"
#' @param round_digit round estimates, default to 1
#' @param use_sample_data use a build-in sample data for fast evaluating the
#'   function
#'
#' @return a data.table
#' @export get.CME.UI.data
#'
#' @examples
#' dt_t <- get.CME.UI.data(c_iso = "AFG", year_range = 2000:2018,
#' ind = "all", use_sample_data = TRUE)
#' dt_f <- get.CME.UI.data(c_iso = "AFG", year_range = 1960:2018,
#' ind = c("U5MR", "IMR"), format = "wide_year", use_sample_data = TRUE)
#'
#'
get.CME.UI.data <- function(
  c_iso = NULL,
  year_range = c(1990:2018),
  ind = "all",
  get = "rate",
  idvars = c("OfficialName", "ISO3Code"),
  dir_file = NULL,
  match_ind = NULL,
  format = "long",
  round_digit = 1L,
  use_sample_data = FALSE
){
  if(is.null(dir_file)) {
    dt <- if(use_sample_data) {
      dt_sample
    } else {
      fread(dir_list$dir_total_2019)
      message("Data loaded (by default), supply dir_file if needed: ",
              dir_list$dir_total_2019)
    }
  } else {
    if(file.exists(dir_file)){
      dt <- fread(dir_file)
      message("Data loaded: ", dir_file)
    } else {
      stop("Check: file doesn't exist: ", dir_file)
    }
  }
  available_years <- as.numeric(sub("IMR.", "", (grep("IMR", names(dt), value = TRUE))))
  if (!all(year_range%in%available_years)) {
    stop("Available years are between: ", paste(range(available_years), collapse = " and "),
         ". Set years to default range.")
  }

  if(!all(ind%in%c("U5MR", "NMR", "IMR", "all"))) stop("choose indicators among U5MR, NMR, and IMR")
  if("all" %in% ind) ind <- c("U5MR", "NMR", "IMR")

  match_ind_default <- list(
    # "U5MR" = "Under-five mortality rate",
    # "NMR"  = "Neonatal mortality rate",
    # "IMR"  = "Infant mortality rate",
    "U5MR_death" = "Under.five.Deaths",
    "IMR_death"  = "Infant.Deaths",
    "NMR_death"  = "Neonatal.Deaths"
  )
  if(is.null(match_ind)) match_ind <- match_ind_default
  if(get == "both"){
    # the variables I want from the dataset:
    inds_wanted <- c(get.match(ind, new_list = match_ind), get.match(paste0(ind, "_death"), new_list = match_ind))
  } else if (get == "rate") {
    inds_wanted <- get.match(ind, new_list = match_ind)
  } else if (get == "death") {
    inds_wanted <- get.match(paste0(ind, "_death"), new_list = match_ind)
  } else {
    stop("get either rate, death, or both")
  }

  isos <- dt[, unique(ISO3Code)]
  isos <- isos[isos!=""]
  if(!is.null(c_iso)) {
    c_iso <- c_iso[c_iso%in%isos]
  } else {
    c_iso <- isos
  }
  inds_wanted_year <- sort(do.call(paste, c(expand.grid(inds_wanted, year_range), sep = ".")))
  # "X" is the quantile column
  vars_wanted <- c(idvars, "X", inds_wanted_year)
  dt1 <- dt[ISO3Code %in% c_iso,..vars_wanted]
  if(!format%in%c("raw", "long", "wide_year", "wide_ind", "wide_get")){
    message("Choose format among raw, long, wide_get, wide_year and wide_ind, default to long")
    format <-  "wide_ind"
  }
  if(format == "raw") {
    return(dt1)
  } else {
    dt1[, (inds_wanted_year):= lapply(.SD, as.numeric), .SDcols = inds_wanted_year]
    dt_long <- melt(dt1, measure.vars = inds_wanted_year, value.name = "Value", variable.factor = FALSE)
    # dt_long[, Indicator:= gsub( "\\..*", "", variable )] # remove anything after .
    dt_long[, Indicator:= substr(variable, 1, nchar(variable)-5)] # remove anything after .
    dt_long[, Year:= gsub( ".*\\.", "", variable )] # remove anything after the last dot
    setnames(dt_long, "X", "UI")
    vars_kept <- c(idvars, "Indicator", "Year", "UI", "Value")
    dt_long <- dt_long[, ..vars_kept]
    setkey(dt_long, ISO3Code)
    # round?
    if(is.numeric(round_digit)) dt_long[, Value:= roundoff(Value, digits = round_digit)]

    # pick output format here ----
    # since everything is made from format long
    if(format == "long"){
      return(dt_long)
      # wide all the indicators (incl. rates, deaths, if all selected)
    } else if (format == "wide_ind") {
      # e.g. OfficialName ISO3Code      X Year      U5MR      NMR       IMR
      dt_wide_ind <- dcast(dt_long, ISO3Code + OfficialName  + Year + UI ~ Indicator, value.var = "Value")
      return(dt_wide_ind)
      # wide all the years, all the inds in one column
    } else if (format == "wide_year") {
      dt_wide_year <- dcast(dt_long, ISO3Code + OfficialName  +Indicator  + UI ~ Year, value.var = "Value")
      return(dt_wide_year)
    } else {
      # Two value columns : one for rate, one for deaths
      dt_long[, Get:="Rate"]
      dt_long[grepl("Deaths", Indicator), Get:="Deaths"]
      if(uniqueN(dt_long$Get)==1){
        "You only selected Rate or Death, not both, long-format output is returned"
        return(dt_long)
      } else {
        align_ind <- list(
          "U5MR" = "Under.five",
          "Under.five.Deaths"  = "Under.five",
          "IMR"  = "Infant",
          "Infant.Deaths" = "Infant",
          "NMR"  = "Neonatal",
          "Neonatal.Deaths"  = "Neonatal"
        )
        dt_long[, Indicator:= get.match(Indicator, new_list = align_ind)]
        dt_wide_get <- dcast(dt_long, ISO3Code + OfficialName  + Indicator  + Year + UI~ Get, value.var = "Value")
        return(dt_wide_get)
      }
    }
  }
}


#' older and simpler version: get three types (Under-five Infant Neonatal) from
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
get.CME.data <- function(year_range = c(1990:2018), get_what = "rate",
                             dir_file = NULL){
  dt <- if(is.null(dir_file)) Rates_Deaths_Country_Summary_2019 else fread(dir_file)

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
                                   measure.vars = patterns(paste0("^", CME_types_full)),
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


