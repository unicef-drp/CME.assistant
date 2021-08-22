# Data engineering related helper functions
# CME functions package

# Directories related -------------------------------------------------------------------

#' Search for file paths matched by part of the file name
#'
#' Search for files containing the `file_name_string` in all sub-folders in
#' `target.dir`, and list files containing the `file_name_string`
#'
#' @param target.dir target directory
#' @param file_name_string e.g. "data_U5MR"
#' @param full_path full path or not
search.for.file <- function(target.dir, file_name_string, full_path = FALSE){
  n <- which(grepl(file_name_string, list.files(target.dir, recursive = TRUE)))
  list.files(target.dir, recursive = TRUE, full.names = full_path)[n]
}


#' Get leading path in file directories depending on operation system (Mac OSX or Windows)
#'
#' @return "Users/<username>" or "C:/Users/<username>"
#' @export load_os_leading_dir
#'
#' @examples load_os_leading_dir()
load_os_leading_dir <- function(){
  user <- Sys.info()[["user"]]
  os <- get_os()
  if(!os %in% c("windows", "osx")) warning ("Have only saved directories for Windows and Mac OSX")
  leading_path <- if(os == "osx") file.path("Users", user) else Sys.getenv("USERPROFILE")
  return(leading_path)
}


#' Return a saved dir_list for total and sex-specific Rates & Deaths_Country
#' Summary.csv
#'
#' `load.final_dir` returns file paths of Rates & Deaths_Country Summary.csv
#' from Aggregate results (final) folders
#'
#' @param year default to 2020, can get directory list for 2020 or 2019,
#'   otherwise return both years
#'
#' @return list of results file directories on Dropbox
#' @export load.final_dir
load.final_dir <- function(year = 2020){
  leading_path <- load_os_leading_dir()
  y19 <- list(
    dir_total_2019 = file.path(leading_path,
                               "/Dropbox/UN IGME Data/2019 Round Estimation/Code/Aggregate results (final) 2019-08-15/Rates & Deaths_Country Summary.csv"),
    dir_female_2019 = file.path(leading_path,
                               "/Dropbox/UN IGME Data/2019 Round Estimation/Code/Aggregate results (final) 2019-08-20 (female)/Rates & Deaths(ADJUSTED)_female_Country Summary (negative replaced).csv"),
    dir_male_2019 = file.path(leading_path,
                               "/Dropbox/UN IGME Data/2019 Round Estimation/Code/Aggregate results (final) 2019-08-20 (male)/Rates & Deaths(ADJUSTED)_male_Country Summary.csv"),
    dir_5_14_2019 = file.path(leading_path,
                              "Dropbox/IGME 5-14/2019 Round Estimation/Aggregate results (final) 2019-07-29/Rates & Deaths_Country Summary.csv")

  )
  y20 <- list(
    dir_total_2020 = file.path(leading_path,
                              "/Dropbox/UN IGME Data/2020 Round Estimation/Code/Aggregate results (final) 2020-08-14/Rates & Deaths_Country Summary.csv"),
    dir_female_2020 = file.path(leading_path,
                              "/Dropbox/UN IGME Data/2020 Round Estimation/Code/Aggregate results (final) 2020-08-16 (female)/Rates & Deaths(ADJUSTED)_female_Country Summary.csv"),
    dir_male_2020 = file.path(leading_path,
                              "/Dropbox/UN IGME Data/2020 Round Estimation/Code/Aggregate results (final) 2020-08-16 (male)/Rates & Deaths(ADJUSTED)_male_Country Summary.csv"),
    dir_5_14_2020 = file.path(leading_path,
                              "/Dropbox/IGME 5-14/2020 Round Estimation 10q5/Aggregate results (final) 2020-08-31/Rates & Deaths_Country Summary.csv"),
    dir_15_24_2020 = file.path(leading_path,
                              "/Dropbox/IGME 15-24/2020 Round Estimation 10q15/Aggregate results (final) 2020-08-31/Rates & Deaths_Country Summary.csv")
    #
  )
  year <- as.character(year)
  if (year == "2020") {
    dir_list <- y20
  } else if (year == "2019") {
    dir_list <- y19
  } else {
    dir_list <- c(y19, y20)
  }
  # check if all the files still exist (if file names remain unchanged)
  if(!all(sapply(dir_list, file.exists))) warning("Check file paths that don't exist:\n ", paste(dir_list[!sapply(dir_list, file.exists)], collapse = ",\n "))
  return(dir_list)
}



# General helper -----------------------------------------------------------

#' Check and install packages if missing
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


# Get data ---------------------------------------------------------------

#' Return different format of the final aggregate results country summary
#'
#' `get.CME.UI.data` can read in "Rates & Deaths_Country Summary.csv" for any
#' indicators published so far and include Sex, Quantile in the output. If there
#' is only median (i.e. no Quantile column in the dataset) the function will
#' check if there is only one row per country. A pre-saved list of summary.csv
#' files directories could be obtained by \code{\link{load.final_dir}}. Choose
#' the `format` of the output dataset from `long`, `wide_year`, `wide_ind`
#' (wide by indicator) and `wide_get` (one column for rate and one column for
#' death)
#'
#' @param dir_file directory to the dataset to be read: directory to aggregate
#'   final
#' @param c_iso country ISO3Code, default to NULL: returns all countries in the
#'   dataset
#' @param year_range a vector of years, default to 1990: 2019
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
  year_range = c(1990:2019),
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
    message("`year_range` set to NULL: use all available years in the dataset.")
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
      if(grepl("female", dir_file)){
        sex <- "Female"
      } else if (grepl("male", dir_file)) {
        sex <- "Male"
      } else {
        sex <- "Total"
      }
    }
  }
  dt[, Sex:= sex]

  if(!format%in%c("raw", "long", "wide_year", "wide_ind", "wide_get")){
    message("Choose format among raw, long, wide_get, wide_year and wide_ind, default to long")
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
      formula0 <- paste(paste(idvars, collapse = " + "), "+ Year + Quantile + Sex ~ Shortind" )
      dt_wide_ind <- data.table::dcast.data.table(dt_long, formula = formula0, value.var = "value")
      return(dt_wide_ind)
      # wide all the years, all the inds in one column
    } else if (format == "wide_year") {
      formula0 <- paste(paste(idvars, collapse = " + "), " + Shortind  + Quantile + Sex ~ Year" )
      dt_wide_year <- data.table::dcast.data.table(dt_long, formula = formula0, value.var = "value")
      return(dt_wide_year)
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
        formula0 <- paste(paste(idvars, collapse = " + "), "+ Shortind  + Year + Quantile + Sex ~ Get" )
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
#' \code{\link{get.CME.UI.data}} in the code if needed
#'
#' @param dir_file directory to Rates & Deaths_Country Summary.csv
#' @param year_range default to null, supply e.g. 1990:2019
#' @param sex default to NULL, will determine from file dir
#' @export
read.country.summary <- function(
  dir_file,      # fread("Rates & Deaths_Country Summary.csv)
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
    message("`year_range` set to NULL: use all available years in the dataset.")
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
    if(grepl("female", dir_file)){
      sex <- "Female"
    } else if (grepl("male", dir_file)) {
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
    message("`year_range` set to NULL: use all available years in the dataset.")
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
    if(grepl("female", dir_file)){
      sex <- "Female"
    } else if (grepl("male", dir_file)) {
      sex <- "Male"
    } else {
      sex <- "Total"
    }
  }
  dt_long[, Sex:= sex]
  return(dt_long)
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
