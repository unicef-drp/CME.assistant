# Data engineering related helper functions


# Get data ---------------------------------------------------------------

#' Return different format of the final aggregate results country summary
#'
#' `get.CME.UI.data` can read in "Rates & Deaths_Country Summary.csv" for any
#' indicators published so far and include Sex, Quantile in the output. If there
#' is only median (i.e. no Quantile column in the dataset) the function will
#' check if there is only one row per country. A pre-saved list of summary.csv
#' files directories could be obtained by \code{\link{load.final_dir}}. Choose
#' the `format` of the output dataset from `long`, `wide_q`(wide quantile),
#' `wide_year`, `wide_ind` (wide indicator) and `wide_get` (one column for rate
#' and one column for death)
#'
#' @param dir_file directory to the dataset to be read: directory to aggregate
#'   final
#' @param c_iso country ISO3Code, default to NULL: returns all countries in the
#'   dataset
#' @param year_range a vector of years, default to NULL: use all available years
#' @param idvars default to "`OfficialName`, `ISO3Code`", what id vars you want
#'   to include
#' @param format Choose format among raw, long, wide_year, wide_ind, and
#'   wide_get, default to "long". All the wide-format just `dcasts` the
#'   long-format data, wide_get means two columns: rate and death
#' @param get  default to "rate". Choose among "rate", "death", or "both"
#' @param round_digit digits to round estimates, default to `NULL`
#' @param use_IGME_year load the saved IGME final aggregated results (final):
#'   choose between 2019 or 2020
#' @param quantile default to TRUE: return upper, median, lower; FALSE: only
#'   median.
#' @param sex default as NULL, will determine sex from directory of `dir_file`
#'   unless supplied
#'
#' @return a data.table (data.frame)
#' @export get.CME.UI.data
#' @examples
#' dt_1 <- get.CME.UI.data(use_IGME_year = 2020, format = "wide_year")
#' dt_2 <- get.CME.UI.data(format = "wide_ind")
#'
#'
get.CME.UI.data <- function(
  dir_file = NULL,
  c_iso = NULL,
  year_range = NULL,
  get = "both", # "rate" / "death" / "both"
  idvars = c("OfficialName", "ISO3Code"),
  sex = NULL,
  format = "long",  # "raw"/ "long"/ "wide_year"/ "wide_ind"/ "wide_get"
  round_digit = NULL,
  use_IGME_year = 2020,
  quantile = TRUE
){
  # load file
  if(is.null(dir_file)) {
    if(use_IGME_year==2020){
      dt <- Rates_Deaths_Country_Summary_2020_UI
    } else if (use_IGME_year == 2019){
      dt <- Rates_Deaths_Country_Summary_2019_UI
    } else {
      stop("Supply dir_file or choose `use_IGME_year` among 2019, 2020")
    }
  } else {
    dir_file <- unlist(dir_file)
    if(file.exists(unlist(dir_file))){
      dt <- data.table::fread(dir_file)
      message("Data loaded: ", dir_file)
    } else {
      stop("Check: file doesn't exist: ", dir_file)
    }
  }
  # cleaning up col names
  dt <- dt[ISO3Code!="LIE"]
  setnames(dt, gsub(" ", ".", colnames(dt)))
  setnames(dt, gsub("-", ".", colnames(dt)))

  # find the Quantile column:

  if("Quintile"%in%colnames(dt))setnames(dt, "Quintile", "Quantile")
  if("X"%in%colnames(dt))setnames(dt, "X", "Quantile")
  # in case leave as blank, column will have names like V99, V101, etc
  if(!"Quantile"%in%colnames(dt)){
    columnV <- grep("V", colnames(dt), value = TRUE)
    columnV <- columnV[which(nchar(columnV) %in% c(3,4))]
    message("Assign this column as Quantile column: ", columnV[1])
    setnames(dt, columnV[1], "Quantile")
  }

  # `row_per_iso` should be either 1 or 3, so `length(row_per_iso)` should be 1
  row_per_iso <- unique(dt[,.N, by = ISO3Code][,N])
  if(length(row_per_iso)!=1) warning("Should be one row each country? Check row per iso: ", paste(row_per_iso, collapse = ", "))

  # Does the quantile column exist?
  # If there is no Quantile column, then I assume there is only Median.
  # There should one row per country
  if(!"Quantile"%in%colnames(dt)){
    # If there is no quantile column it means there should be median only
    if(row_per_iso==1){
      dt[, Quantile:= "Median"]
    } else {
      stop("There should be a `Quantile` column in the final results file?")
    }
  }

  # find all available indicators
  # get all the variables available in the datasets:
  # e.g. c("X5q15", "X10q15", "X5q20")
  available_inds <- grep(".2018", colnames(dt), value = TRUE, fixed = TRUE)
  available_inds <- gsub(".2018", "", available_inds)

  if(get == "rate"){
    inds_wanted <- available_inds[!grepl("death|Death", available_inds)]
  } else if (get == "death"){
    inds_wanted <- available_inds[grepl("death|Death", available_inds)]
  } else {
    inds_wanted <- available_inds # otherwise just take all available inds
  }

  # find all available years
  # available years
  available_years <- grep(inds_wanted[1], colnames(dt), value = TRUE, fixed = TRUE)
  available_years <- as.numeric(gsub(paste0(inds_wanted[1], "."), "", available_years))
  if(!is.null(year_range)){
    # so it is OK to supply years by mistake like year_range = 2000.4, match by
    # flooring
    year_range <- available_years[floor(available_years)%in%floor(as.numeric(year_range))]
    if(length(year_range)==0){
      message("Supplied `year_range` is not in available years.\n",
              "Available years are between ", paste(range(available_years), collapse = " and "),
              " --- will use all available years")
      year_range <- available_years
    }
  } else {
    message("`year_range` set to NULL: use all available years in the dataset: ",
            paste(range(available_years), collapse = "-"))
    year_range <- available_years
  }

  # subset isos if what's required is available
  isos <- dt[, unique(ISO3Code)]
  isos <- isos[isos!=""]
  if(!is.null(c_iso)) {
    c_iso <- c_iso[c_iso%in%isos]
  } else {
    c_iso <- isos
  }

  # determine sex from dir
  if(is.null(dir_file)){
    sex <- "Total"
  } else {
    if(is.null(sex)){
      if(grepl("female", dir_file, ignore.case = TRUE)){
        sex <- "Female"
      } else if (grepl("male", dir_file, ignore.case = TRUE)) {
        sex <- "Male"
      } else {
        sex <- "Total"
      }
    }
  }
  dt[, Sex:= sex]

  if(!format%in%c("raw", "long", "wide_q", "wide_year", "wide_ind", "wide_get")){
    message("Choose format among raw, long, wide_q, wide_get, wide_year and wide_ind, default to long")
    format <-  "long"
  }
  # the value variable
  inds_wanted_year <- sort(do.call(paste, c(expand.grid(inds_wanted, year_range), sep = ".")))
  dt[, (inds_wanted_year):= lapply(.SD, as.numeric), .SDcols = inds_wanted_year]
  #
  vars_wanted <- c(idvars, "Quantile", "Sex", inds_wanted_year)
  dt1 <- dt[ISO3Code %in% c_iso, ..vars_wanted]

  # quantile
  if(!is.logical(quantile)){
    quantile <- TRUE
    message("quantile default to TRUE, choose between TRUE or FALSE.")
  }
  if(!quantile) dt1 <- dt1[Quantile=="Median"]

  if(format == "raw") {
    return(dt1)
  } else {
    dt_long <- data.table::melt(dt1, measure.vars = inds_wanted_year, value.name = "value", variable.factor = FALSE)
    # dt_long[, Shortind:= gsub( "\\..*", "", variable )] # remove anything after .
    dt_long[, Shortind:= substr(variable, 1, nchar(variable)-5)] # remove anything after .
    # Deaths -> deaths
    dt_long[, Shortind:= gsub("Deaths", "deaths", Shortind)]
    message("Shortind: ", paste(dt_long[, unique(Shortind)], collapse = ", "))

    dt_long[, Year:= gsub( ".*\\.", "", variable )] # remove anything after the last dot
    dt_long[, Year:= as.numeric(Year)]
    vars_kept <- c(idvars, "Shortind", "Year", "Quantile", "Sex", "value")
    dt_long <- dt_long[, ..vars_kept]
    data.table::setkey(dt_long, ISO3Code)
    # round?
    if(is.numeric(round_digit)) dt_long[, value:= roundoff(value, digits = round_digit)]
    # remove NA
    dt_long <- dt_long[!is.na(value)]

    # can choose output format ----
    # since everything is made from format long
    if(format == "long"){
      return(dt_long)
      # wide all the indicators (incl. rates, deaths, if all selected)
    } else if (format == "wide_ind") {
      # e.g. OfficialName    ISO3Code      X Year      U5MR      NMR       IMR
      formula0 <- paste(paste(idvars, collapse = " + "), " + Sex + Year + Quantile ~ Shortind" )
      dt_wide_ind <- data.table::dcast.data.table(dt_long, formula = formula0, value.var = "value")
      return(dt_wide_ind)
      # wide all the years, all the inds in one column
    } else if (format == "wide_year") {
      formula0 <- paste(paste(idvars, collapse = " + "), " + Shortind  + Sex  + Quantile ~ Year" )
      dt_wide_year <- data.table::dcast.data.table(dt_long, formula = formula0, value.var = "value")
      return(dt_wide_year)
    } else if (format == "wide_q") {
      formula0 <- paste(paste(idvars, collapse = " + "), "+ Shortind + Sex + Year ~ Quantile" )
      dt_wide_quantile <- data.table::dcast.data.table(dt_long, formula = formula0, value.var = "value")
      return(dt_wide_quantile)
    } else {
      # Two value columns : one for rate, one for deaths
      dt_long[, Get:="Rate"]
      dt_long[grepl("deaths", Shortind, ignore.case = TRUE), Get:="Deaths"]
      if(data.table::uniqueN(dt_long$Get)==1){
        "You only selected Rate or Death, not both, so long-format output is returned"
        return(dt_long)
      } else {
        align_ind <- list(
          "U5MR" = "Under.five",
          "Under.five.deaths"  = "Under.five",
          "IMR"  = "Infant",
          "Infant.deaths" = "Infant",
          "NMR"  = "Neonatal",
          "Neonatal.deaths"  = "Neonatal"
        )
        dt_long[, Shortind:= get.match(Shortind, new_list = align_ind)]
        formula0 <- paste(paste(idvars, collapse = " + "), "+ Shortind  + Sex + Year + Quantile  ~ Get" )
        dt_wide_get <- data.table::dcast.data.table(dt_long, formula = formula0, value.var = "value")
        return(dt_wide_get)
      }
    }
  }
}


#' Read "Rates & Deaths_Country Summary.csv" and output long format
#'
#' This function is a simpler version of \code{\link{get.CME.UI.data}} and is
#' more straightforward to use: without many arguments, only output long format.
#' The arguments are aligned so this function could be replaced by
#' \code{\link{get.CME.UI.data}} in the code if needed.
#' \code{\link{get.CME.UI.data}} offers more options, including output format,
#' selecting id.vars. etc
#'
#' @param dir_file directory to Rates & Deaths_Country Summary.csv
#' @param year_range default to null, supply e.g. 1990:2019
#' @param sex default to NULL, will determine from file dir
#' @export
read.country.summary <- function(
  dir_file,      # directory to "Rates & Deaths_Country Summary.csv"
  year_range = NULL, # e.g. 1990:2019
  sex = NULL
){
  if(!file.exists(dir_file)) stop("File doesn't exist: ", dir_file)
  dt_cs <- fread(dir_file)[ISO3Code!="LIE"]
  # clean up column names
  setnames(dt_cs, gsub(" ", ".", colnames(dt_cs)))
  setnames(dt_cs, gsub("-", ".", colnames(dt_cs)))

  # find the Quantile column:
  if("Quintile"%in%colnames(dt_cs))setnames(dt_cs, "Quintile", "Quantile")
  if("X"%in%colnames(dt_cs))setnames(dt_cs, "X", "Quantile")
  # in case leave as blank, column will have names like V99, V101, etc
  if(!"Quantile"%in%colnames(dt_cs)){
    columnV <- grep("V", colnames(dt_cs), value = TRUE)
    columnV <- columnV[which(nchar(columnV) %in% c(3,4))]
    message("Assign this column as Quantile column: ", columnV[1])
    setnames(dt_cs, columnV[1], "Quantile")
  }

  # get all the variables available in the datasets:
  # e.g. c("X5q15", "X10q15", "X5q20")
  vars <- grep(".2019", colnames(dt_cs), value = TRUE, fixed = TRUE)
  vars <- gsub(".2019", "", vars)
  # available years
  available_years <- grep(vars[1], colnames(dt_cs), value = TRUE, fixed = TRUE)
  available_years <- as.numeric(gsub(paste0(vars[1], "."), "", available_years))
  if(!is.null(year_range)){
    # so it is OK to supply years by mistake like year_range = 2000.4, match by
    # flooring
    year_range <- available_years[floor(available_years)%in%floor(as.numeric(year_range))]
    if(length(year_range)==0){
      message("Supplied `year_range` is not in available years.\n",
              "Available years are between ", paste(range(available_years), collapse = " and "),
              " --- will use all available years")
      year_range <- available_years
    }
  } else {
    message("`year_range` set to NULL: use all available years in the dataset: ", paste(range(available_years), collapse = "-"))
    year_range <- available_years
  }

  vars_wanted <- do.call(paste0, expand.grid(vars, ".", year_range))
  dt_cs <- dt_cs[, c("ISO3Code", "Quantile", vars_wanted), with = FALSE]
  dt_cs[, (vars_wanted):=lapply(.SD, as.numeric), .SDcols = vars_wanted]
  dt_long <- melt.data.table(dt_cs, id.vars = c("ISO3Code", "Quantile"),
                             variable.factor = FALSE)
  dt_long[, Year := as.numeric(substr(variable, nchar(variable)-3, nchar(variable)))]
  dt_long[, Shortind := substr(variable, 1, nchar(variable)-5)]
  dt_long[, Shortind := gsub("Deaths", "deaths", Shortind)]
  dt_long[, variable:= NULL]
  # determine sex from dir
  if(is.null(sex)){
    if(grepl("female", dir_file, ignore.case = TRUE)){
      sex <- "Female"
    } else if (grepl("male", dir_file, ignore.case = TRUE)) {
      sex <- "Male"
    } else {
      sex <- "Total"
    }
  }
  dt_long[, Sex:= sex]
  return(dt_long)
}

#' Read regional summary and output long format

#' @param dir_file directory to e.g. paste0("Rates & Deaths_UNICEFRegion.csv")
#'
#' @param year_range default to null, supply e.g. 1990:2019
#' @param add_regional_grouping if add `Regional_Grouping` column
#' @param sex default to NULL, will determine from file dir
#'
#' @export
read.region.summary <- function(
  dir_file,      # regional summary in aggregate results
  year_range = NULL,# e.g. c(1990, 2019)
  sex = NULL,
  add_regional_grouping = FALSE
){
  if(!file.exists(dir_file)) stop("File doesn't exist: ", dir_file)
  dt_cs <- fread(dir_file)
  # What region is it?

  if(grepl("SDG", dir_file)) Regional_Grouping <- "SDG"
  if(grepl("UNICEF", dir_file)) Regional_Grouping <- "UNICEF"
  if(grepl("WB", dir_file)) Regional_Grouping <- "World Bank"

  # clean up the col names
  setnames(dt_cs, gsub(" ", ".", colnames(dt_cs)))
  setnames(dt_cs, gsub("-", ".", colnames(dt_cs)))
  #
  vars0 <- colnames(dt_cs)
  vars0 <- vars0[!grepl("Population", vars0, ignore.case = TRUE)]
  vars_wanted <- vars0[!vars0%in%c("Region", "Year")]
  # available years
  available_years <- sort(unique(dt_cs$Year))
  if(!is.null(year_range)){
    # so it is OK to supply years by mistake like year_range = 2000.4, match by
    # flooring
    year_range <- available_years[floor(available_years)%in%floor(as.numeric(year_range))]
    if(length(year_range)==0){
      message("Supplied `year_range` is not in available years.\n",
              "Available years are between ", paste(range(available_years), collapse = " and "),
              " --- will use all available years")
      year_range <- available_years
    }
  } else {
    message("`year_range` set to NULL: use all available years in the dataset: ", paste(range(available_years), collapse = "-"))
    year_range <- available_years
  }
  dt_cs <- dt_cs[Year%in%year_range]
  dt_cs[, (vars_wanted):=lapply(.SD, as.numeric), .SDcols = vars_wanted]
  if("Region" %in% colnames(dt_cs)){
    id_vars <- c("Region", "Year")
  } else {
    id_vars <- c("Year")
    message("There is no `Region` column, assume this is world results")
  }
  dt_long <- melt.data.table(dt_cs[,..vars0], id.vars = id_vars, variable.factor = FALSE)
  dt_long[grepl("upper", variable, ignore.case = TRUE), Quantile := "Upper"]
  dt_long[grepl("median", variable, ignore.case = TRUE), Quantile := "Median"]
  dt_long[grepl("lower", variable, ignore.case = TRUE), Quantile := "Lower"]
  dt_long[, Shortind := gsub("X|.lower.bound|.upper.bound|.median", "", variable)]
  dt_long[, Shortind := gsub("Deaths", "deaths", Shortind)]

  dt_long[, variable:= NULL]
  if(add_regional_grouping) dt_long[, Regional_Grouping:= Regional_Grouping]
  dt_long[, Year:= floor(Year) + 0.5]
  # determine sex from dir
  if(is.null(sex)){
    if(grepl("female", dir_file, ignore.case = TRUE)){
      sex <- "Female"
    } else if (grepl("male", dir_file, ignore.case = TRUE)) {
      sex <- "Male"
    } else {
      sex <- "Total"
    }
  }
  dt_long[, Sex:= sex]
  return(dt_long)
}



#' Read one results.csv file and reformat into long-format
#'
#' @param dt_dir the single directory to a results.csv file
#' @param year_range year range desired, default to all years: `1931:2030`
#' @param q quantile desired, default to `c("Lower", "Median", "Upper")`
#' @param sex default to NULL, sex is determined from `dt_dir`, unless specified
#' @return long-format results.csv, all values in the "value" column
#' @export read.results.csv
read.results.csv <- function(
  dt_dir,
  year_range = 1931:2030,
  q = c("Lower", "Median", "Upper"),
  sex = NULL
){
  id_vars <- c("ISO.Code", "Quantile", "Indicator")
  if(grepl(".xlsx|.xls", dt_dir)){
    # dt1 <- setDT(readxl::read_xlsx(dt_dir))
    stop("dt_dir points to xlsx file.")
  } else {
    dt1 <- fread(dt_dir, header = TRUE)
  }
  dt1 <- dt1[Quantile %in% q, ]
  # align the column names first
  if("ISO Code"%in%colnames(dt1)) setnames(dt1, "ISO Code", "ISO.Code")

  if(!is.numeric(year_range)){
    year_range <- 1931:2030
    message("`year_range` set to default: 1931-2030")
  }

  if(any(grepl("X", colnames(dt1)))){
    value_vars <- paste0("X", year_range, ".5")
  } else if (any(grepl(".5", colnames(dt1), fixed = TRUE))) {
    value_vars <- paste0(year_range, ".5")
  } else {
    value_vars <- paste0(year_range)
  }
  value_vars <- value_vars[value_vars%in%colnames(dt1)] # only available years will be picked
  dt1[, (value_vars):= lapply(.SD, as.numeric), .SDcols = value_vars]
  # can also assign indicator for P-factor: P5/4/1_country.csv for under-five
  # cannot assign automatically for older children
  ind0 <- dt1$Indicator[1]
  if(grepl("U5MR|P5", ind0)) dt1$Indicator <- "Under-five Mortality Rate"
  if(grepl("IMR|P1",  ind0)) dt1$Indicator <- "Infant Mortality Rate"
  if(grepl("CMR|P4",  ind0)) dt1$Indicator <- "Child mortality rate age 1-4"

  if(grepl("10q5|5-14",  dt_dir)) dt1$Indicator <- "Mortality rate age 5-14"
  if(grepl("5q5|5-9",    dt_dir)) dt1$Indicator <- "Mortality rate age 5-9"
  if(grepl("5q10|10-14", dt_dir)) dt1$Indicator <- "Mortality rate age 10-14"

  if(grepl("10q15|15-24", dt_dir)) dt1$Indicator <- "Mortality rate age 15-24"
  if(grepl("5q15|15-19",  dt_dir)) dt1$Indicator <- "Mortality rate age 15-19"
  if(grepl("5q20|20-24",  dt_dir)) dt1$Indicator <- "Mortality rate age 20-24"

  # but overwrite for P-factor results if it is older children. Cannot
  # automatically judge indicators since cannot distinguish 5-14 vs 15-24. Need
  # to do it manually later
  if(!(dt1$Indicator[1] %in% c("Under-five Mortality Rate",
                               "Infant Mortality Rate",
                               "Child mortality rate age 1-4")) & grepl("P", dt_dir)) dt1$Indicator <- ind0

  vars_wanted <- c(id_vars, value_vars)
  dt1_long <- melt.data.table(dt1[,..vars_wanted], measure.vars = value_vars,
                              variable.name = "Year", variable.factor = FALSE)
  dt1_long[, value:=as.numeric(value)]
  if(any(grepl("X", colnames(dt1)))) {
    dt1_long[, Year:= floor(as.numeric(sub("X","",Year)))]
  } else {
    dt1_long[, Year:= floor(as.numeric(Year))]
  }

  # determine sex from dir
  if(is.null(sex)){
    if(grepl("female", dt_dir, ignore.case = TRUE)){
      sex <- "Female"
    } else if (grepl("male", dt_dir, ignore.case = TRUE)) {
      sex <- "Male"
    }  else if (grepl("P", dt_dir)) {
      sex <- "P_factor"
    } else if (grepl("Sex ratio", dt_dir)) {
      sex <- "Sex_Ratio"
    } else {
      sex <- "Total"
    }

    if(grepl("expected", dt_dir, ignore.case = TRUE)) sex <- paste(sex, "expected")
  }

  dt1_long[, Sex:= sex]


  ind_list <- list(
    "Under-five Mortality Rate" = "U5MR",
    "Infant Mortality Rate" = "IMR",
    "Child mortality rate age 1-4" = "CMR",
    "Neonatal Mortality Rate" = "NMR",

    "Mortality rate age 5-14"  = "10q5",
    "Mortality rate age 5-9"   = "5q5",
    "Mortality rate age 10-14" = "5q10",
    "Mortality rate age 15-24" = "10q15",
    "Mortality rate age 15-19" = "5q15",
    "Mortality rate age 20-24" = "5q20"

  )
  dt1_long[, Shortind:= get.match(Indicator, new_list = ind_list)]

  setkey(dt1_long, ISO.Code, Year)
  dt1_long <- dt1_long[,.(ISO.Code, Shortind, Indicator, Sex, Year, Quantile, value)]
  return(dt1_long)
}




#' Read and bind all "results.csv" into one file using `rbindlist`
#'
#' Use \code{\link{read.results.csv}} to read all "results.csv" files
#'
#' @param results_dir_list results_dir_list list of all the results.csv files to
#'   read in and compare
#' @param year_range0 year range, default to years `1931:2021`
#' @param value_name name of the value, default to "Results"
#'
#' @return long-format results.csv for all indicators, all values in the
#'   `value_name` column
#' @export read.all.results.csv
read.all.results.csv <- function(
  results_dir_list,
  year_range0 = 1931:2020,
  value_name = "Results"
){
  # a list of all the results files:
  # combine original results
  dt_results_2020 <- rbindlist(lapply(results_dir_list, read.results.csv, year_range = year_range0))
  setnames(dt_results_2020, "value", value_name)
  # dt_results_2020[, Quantile:= as.factor(Quantile)]
  # if(!identical(levels(dt_results_2020$Quantile), c("Lower", "Median", "Upper"))) stop("Check Quantile levels: ", paste(levels(dt_results_2020$Quantile), collapse = ", "))
  # dt_results_2020$Quantile <- factor(dt_results_2020$Quantile, levels = c("Median", "Lower", "Upper"))
  return(dt_results_2020)
}



#' Load the "country.info.CME"
#'
#' Creates UNICEFReportRegion from UNICEFReportRegion1 and UNICEFReportRegion2
#'
#' @param year0 IGME round, e.g. 2021
#'
#' @import data.table
#' @export get.country.info.CME
#' @return dataset of country info
get.country.info.CME <- function(year0 = 2021){
  dir_input <- get.IGMEinput.dir(year = year0)
  dc <- fread(file.path(dir_input, "country.info.CME.csv"))
  # UNICEFReportRegion2 offers subregions for ECA and SSA, combined into UNICEFReportRegion
  dc[, UNICEFReportRegion:= ifelse(UNICEFReportRegion2 == "", UNICEFReportRegion1, UNICEFReportRegion2)]
  return(dc)
}


#' Load the "data_livebirths.csv", add ISO3Code and country names
#'
#' @param year0 IGME round, e.g. 2021
#' @export get.live.birth
#' @return dataset of live birth
get.live.birth <- function(year0 = 2021){
  dir_input <- get.IGMEinput.dir(year = year0)
  dc <- fread(file.path(dir_input, "country.info.CME.csv"))
  dtlb <- fread(file.path(dir_input, "data_livebirths.csv"))
  dtlb <- merge(dc[,.(ISO3Code, CountryName, OfficialName, UNCode)], dtlb, by.x = "UNCode", by.y = "uncode")
  return(dtlb)
}




# General helper -----------------------------------------------------------

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


#' Capitalize first letter of each word in the vector
#' @param y vector of strings
#' @return a vector of strings with first letter capitalized
#' @export upper.first.letter
#' @examples upper.first.letter(c("aa","bb","cc"))
upper.first.letter <- function(y){
  if(!is.character(y)) y <- as.character(y)
  upper.first.letter0 <- function(y) {
    c <- strsplit(y, " ")[[1]]
    paste(toupper(substring(c, 1, 1)), substring(c, 2),
          sep="", collapse=" ")
  }
  return(unname(sapply(y, upper.first.letter0)))
}

#' A label function to replace values by a given list in a variable
#'
#' You can provide a __new_list__ to define the values you wish to change in
#' this variable. Values not revised in the given list will be kept
#'
#' @param x a element or a vector
#' @param new_list if you supply a new list the function will use instead of the
#'   default_labels
#' @param show_no_match default to FALSE, if TRUE will message unmatched
#'   elements
#' @param no_line_break default to FALSE, if TRUE will remove line break from
#'   the string
#'
#' @export get.match
#' @return an updated vector as character
get.match <- function(x,
                      new_list = NULL,
                      no_line_break = FALSE,
                      show_no_match = FALSE){
  if(is.null(new_list)){
    labs <- default_label
  } else {
    if(is.list(new_list)){
      labs <- new_list
    } else {
      message("new_list must be a list. Still use the default list.")
      labs <- default_label
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
      if(show_no_match) message("Notice from `get.match`: unmatched input: ", x[i])
    }else{
      out[i] <- labs[[ x[i] ]]
    }
  }
  return(if(no_line_break)gsub("\n", "", out) else out)
}



#' Check and attach libraries. Install if not.
#' @importFrom utils install.packages
#' @param pkgs vector of packages
#' @return NULL
#' @export check.and.install.pkgs
#'
check.and.install.pkgs <- function(pkgs){
  search_package <- sapply(pkgs, find.package, quiet = TRUE) # return a string or character(0)
  new.packages <- pkgs[sapply(search_package, function(x)length(x)==0)]
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
}
