# functions to search and get all major datasets

#' Get IGME "Code" dir for a given year
#'
#' If `year` is 2020, returns the directory to Code folder in the 2020 Round
#' Estimation Dropbox folder
#' @param year YYYY
#' @return directory to input folder
#' @export get.IGME.dir
get.IGME.dir <- function(year){
  USERPROFILE <- load_os_leading_dir()
  file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", year ," Round Estimation/Code/"))
}


#' Load the IGME "input" directories
#'
#' @return dir_IGMEinput_list: a list of directories of UN IGME Data/YYYY Round
#'   Estimations/Code/input
#' @export load.IGMEinput.dir
load.IGMEinput.dir <- function(){
  # the input folder:
  USERPROFILE <- load_os_leading_dir()
  dir_IGMEinput_list <- list(
    dir_IGME_thisyear = file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", format(Sys.Date(), "%Y") ," Round Estimation/Code/input/")),
    dir_IGME_21       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2021 Round Estimation/Code/input/"),
    dir_IGME_20       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2020 Round Estimation/Code/input/"),
    dir_IGME_19       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2019 Round Estimation/Code/input/"),
    dir_IGME_NMR      = file.path(USERPROFILE, "/Dropbox/NMR/data")
  )
  return(dir_IGMEinput_list)
}


#' Get "input" dir for a given year
#'
#' If `year` is 2020, returns the directory to input folder in the 2020 Round
#' Estimation Dropbox folder
#' @param year YYYY
#' @return directory to input folder
#' @export get.IGMEinput.dir
get.IGMEinput.dir <- function(year){
  USERPROFILE <- load_os_leading_dir()
  file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", year ," Round Estimation/Code/input/"))
}

#' Load the IGME "output" directories
#' @return dir_IGMEoutput_list: a list of directories to UN IGME Data/YYYY Round
#'   Estimations/Code/output
#' @export load.IGMEoutput.dir
load.IGMEoutput.dir <- function(){
  USERPROFILE <- load_os_leading_dir()
  dir_IGMEoutput_list <- list(
    dir_IGME_thisyear = file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", format(Sys.Date(), "%Y") ," Round Estimation/Code/output/")),
    dir_IGME_21       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2021 Round Estimation/Code/output/"),
    dir_IGME_20       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2020 Round Estimation/Code/output/"),
    dir_IGME_19       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2019 Round Estimation/Code/output/"),
    dir_IGME_NMR      = file.path(USERPROFILE, "/Dropbox/NMR/output")
  )
  return(dir_IGMEoutput_list)
}

#' Get "output" dir for a given year
#'
#' If `year` is 2020, returns the directory to output folder in the 2020 Round
#' Estimation Dropbox folder
#'
#' @param year YYYY
#' @return directory to output folder
#' @export get.IGMEoutput.dir
get.IGMEoutput.dir <- function(year){
  USERPROFILE <- load_os_leading_dir()
  file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", year ," Round Estimation/Code/output/"))
}

#' Get "fig" dir for a given year
#'
#' If `year` is 2020, returns the directory to fig folder in the 2020 Round
#' Estimation Dropbox folder
#'
#' @param year YYYY
#' @return directory to fig folder
#' @export get.IGMEfig.dir
get.IGMEfig.dir <- function(year){
  USERPROFILE <- load_os_leading_dir()
  file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", year ," Round Estimation/Code/fig/"))
}

#' Internal function: Check if `date` is a leap year
#'
#' @param date date
leap_year <- function(date){
  if (is.numeric(date)) {
    year <- date
  }
  else {
    year <- year(date)
  }
  (year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0))
}

#' Calculate start, end and average date in decimal from starting/end dates
#'
#' @importFrom data.table year
#' @param date0 date for example: 2020-01-01
#' @param date1 date for example: 2020-12-31
#' @return a list of date start, date end, date average. for example: 2020,
#'   2020.997, 2020.497
#' @export get.ref.date
#' @examples get.ref.date("2020-01-01", "2020-12-31")
get.ref.date <- function(date0,
                         date1){
  date0 <- as.Date(date0)
  date1 <- as.Date(date1)
  date_start <- get.numeric.date(date0)
  date_end <- get.numeric.date(date1)
  date_ave <- get.numeric.date(date0 + difftime(date1, date0)/2)
  date_ave_d <- date0 + difftime(date1, date0)/2
  return(list(date_start=date_start, date_end=date_end, date_ave=date_ave, date_ave_d = date_ave_d))
}

#' Transform date into numeric numbers like 2020.55
#'
#' @param date0 date for example: 2020-01-01
#' @return numeric date for example: 2020.014
#' @export
get.numeric.date <- function(date0){

  get.numeric.date.core <- function(date0){
    if(is.na(date0)) return(NA)
    y1 <- data.table::year(date0)
    n_days1 <- ifelse(leap_year(y1), 366, 365) # e.g. 2020 is a leap year with 366 days
    first_day_of_year <- as.Date(paste(y1, 1, 1, sep = "-")) # use to count diff days
    date_num <- as.double(difftime(date0, first_day_of_year))/n_days1 + y1
    return(date_num)
  }
  # support vector input
  unname(sapply(date0, get.numeric.date.core))
}

# Get database path -------------------------------------------------------

#' Show all file directories within the file directory `dir_file` and matched by
#' pattern `pattern0`
#'
#' Search only the files in the folder, match by `pattern0`, the search is not
#' recursive.
#' @param dir_file directory
#' @param pattern0 string to match file names
#' @param full_name list.files(full.names), if TRUE (default) returns full
#'   directories, if FALSE, return only the file names
#' @return vector of matched file directories
#' @export get.file.name
get.file.name <- function(dir_file,
                          pattern0,
                          full_name = TRUE){

  if(is.null(dir_file))message("dir_file is NULL. Please double check.")
  # if(!dir.exists(dir_file))message("Check if dir_file exists: ", dir_file)
  files <- list.files(dir_file)
  files_full <- list.files(dir_file, full.names = TRUE)
  return(if(full_name)files_full[which(grepl(pattern0, files))] else files[which(grepl(pattern0, files))])
}

#' Internal function to check if the input is date, and figure out which date is
#' the latest
#'
#' @param mydate a vector of dates
#' @return an integer returned by `which.max`
get.max.date <- function(mydate) {
  align.date <- function(mydate){
    if(!is.na(as.Date(mydate, "%Y-%m-%d"))){
      mydate <- as.Date(mydate, "%Y-%m-%d")
    } else if (!is.na(as.Date(mydate, "%Y%m%d"))){
      mydate <- as.Date(mydate, "%Y%m%d")
    } else {
      mydate <- NA
    }
    return(mydate)
  }
  out <- sapply(mydate, align.date)
  return(which.max(out))
}

#' Find out the latest date of all the master files in the directory using the
#' dates in file names
#' @param files file path
#'
find_latest_date <- function(files){
  remove_string <- c("data_U5MR_|.csv|data_IMR_|data_NMR_|_5year|dataset_formodeling_|dataset_forplotting_|SexSpecific-entries_")
  dates <- gsub(remove_string, "", files)
  # screen for valid date string:
  # dates <- c("2015", "20200804", "2020-08-01")
  # return which.max e.g. 2L
  get.max.date(dates)
}


#' Get the file directory with latest date in the filename
#'
#' @param dir_folder The directory to search for files
#' @param pattern_to_match Pattern used to match filename
#' @return file path to the dataset
#' @export get.dir_latest_file
get.dir_latest_file <- function(dir_folder, pattern_to_match){
  files_full <- get.file.name(dir_file = dir_folder, pattern0 = pattern_to_match)
  files <- get.file.name(dir_file = dir_folder, pattern0 = pattern_to_match, full_name = FALSE)
  file_selected <- files_full[find_latest_date(files)]
  if(length(file_selected)!=0){
    message(paste(pattern_to_match, "dataset chosen: \n", file_selected))
    return(file_selected)
  } else {
    message("No corresponding dataset found in: \n ", dir_folder)
    return(NULL)
  }
}


#' Get the U5MR master dataset directory
#'
#' @param dir_IGME The directory to IGME input folder, e.g. ".../2020 Round
#'   Estimation/Code/input/", could be obtained using
#'   \code{\link{get.IGMEinput.dir}}
#' @param pattern_to_match default to "data_U5MR", but can be used generally
#'
#' @return file path to the master dataset
#' @export get.dir_U5MR
get.dir_U5MR <- function(dir_IGME = get.IGMEinput.dir(2021), pattern_to_match = "data_U5MR"){
  files_full <- get.file.name(dir_file = dir_IGME, pattern0 = pattern_to_match)
  files <- get.file.name(dir_file = dir_IGME, pattern0 = pattern_to_match, full_name = FALSE)
  file_selected <- files_full[find_latest_date(files)]
  if(length(file_selected)!=0){
    message(paste("U5MR master dataset chosen: \n", file_selected))
    return(file_selected)
  } else {
    message("No corresponding dataset found in: \n ", dir_IGME)
    return(NULL)
  }
}

#' Get the IMR master dataset directory
#'
#' @param dir_IGME The directory to IGME input folder, e.g. ".../2020 Round
#'   Estimation/Code/input/"
#' @return file path to the master dataset
#' @export get.dir_IMR
get.dir_IMR <- function(dir_IGME = get.IGMEinput.dir(2021)){
  files_full <- get.file.name(dir_file = dir_IGME, pattern0 = "data_IMR")
  files <- get.file.name(dir_file = dir_IGME, pattern0 = "data_IMR", full_name = FALSE)
  file_selected <- files_full[find_latest_date(files)]
  if(length(file_selected)!=0){
    message(paste("IMR master dataset chosen: \n", file_selected))
    return(file_selected)
  } else {
    message("No corresponding dataset found in: \n ", dir_IGME)
    return(NULL)
  }
}

#' Get the NMR master dataset directory
#'
#' Compare to \code{\link{get.dir_U5MR}}, there is need to supply dir_IGME since
#' the dataset location is fixed at "/NMR/data"
#'
#' @param y5 to get the 5-year dataset or not
#' @param dir_IGME_NMR default to "Dropbox/NMR/data"
#'
#' @return file path to the master dataset
#' @export get.dir_NMR
get.dir_NMR <- function(
  y5 = FALSE,
  dir_IGME_NMR = NULL
){
  if(is.null(dir_IGME_NMR)){
    dir_IGME_NMR <- file.path(load_os_leading_dir(), "Dropbox/NMR/data")
  }
  if(y5){
    files_full <- get.file.name(dir_file = dir_IGME_NMR, pattern0 = "data_NMR_")
    files_full <- files_full[grepl("5year", files_full)]
    files <- get.file.name(dir_file = dir_IGME_NMR, pattern0 = "data_NMR_", full_name = FALSE)
    files <- files[grepl("5year", files)]
  } else {
    files_full <- get.file.name(dir_file = dir_IGME_NMR, pattern0 = "data_NMR_")
    files_full <- files_full[!grepl("5year", files_full)]
    files <- get.file.name(dir_file = dir_IGME_NMR, pattern0 = "data_NMR_", full_name = FALSE)
    files <- files[!grepl("5year", files)]
  }
  file_selected <- files_full[find_latest_date(files)]
  if(length(file_selected)!=0){
    message(paste("NMR master dataset chosen: \n", file_selected))
    return(file_selected)
  } else {
    message("No corresponding dataset found in: \n ", dir_IGME)
    return(NULL)
  }
}



#' Get the sex-specific master dataset directory
#'
#' Compare to \code{\link{get.dir_U5MR}}, there is need to supply dir_IGME since
#' the dataset location is fixed at "/CMEgender2015/Database"
#'
#' @param plotting to get the dataset for plotting (if TRUE) or dataset for
#'    modeling (if FALSE)
#' @param dir_IGME_gender default to "/Dropbox/CMEgender2015/Database"
#' @return file path to the master dataset
#' @export get.dir_gender
get.dir_gender <- function(
  plotting = TRUE,
  dir_IGME_gender = NULL
){
  if(is.null(dir_IGME_gender)){
    if(plotting){
      dir_IGME_gender <- file.path(load_os_leading_dir(),"/Dropbox/CMEgender2015/Database")
    } else {
      dir_IGME_gender <- file.path(load_os_leading_dir(),"/Dropbox/CMEgender2015/data/interim")
    }
  }
  if(plotting){
    files_full <- get.file.name(dir_file = dir_IGME_gender, pattern0 = "dataset_forplotting")
    files <- get.file.name(dir_file = dir_IGME_gender, pattern0 = "dataset_forplotting", full_name = FALSE)
  }else{
    files_full <- get.file.name(dir_file = dir_IGME_gender, pattern0 = "dataset_formodeling")
    files <- get.file.name(dir_file = dir_IGME_gender, pattern0 = "dataset_formodeling", full_name = FALSE)
  }
  file_selected <- files_full[find_latest_date(files)]
  if(length(file_selected)!=0){
    message(paste("Sex-specific master dataset chosen: \n", file_selected))
    return(file_selected)
  } else {
    message("No corresponding dataset found in: \n ", dir_IGME)
    return(NULL)
  }
}



# For CMRJack results directories

#' Get optimal file directory from `Output CMRJack` folder
#' @param cname country name
#' @param surveytype folder names like "DHS", "MICS", "NDHS",...
#' @param year year of the survey, e.g. 2015
#' @return xlsx file directory
#' @export get.opt.dir
#' @examples
#' \dontrun{
#' get.opt.dir("Zimbabwe", "DHS", 2015)
#' }
get.opt.dir <- function(
  cname,
  surveytype = "DHS",
  year = NULL){
  cname <- gsub(" ", "", cname)
  dir_opt <- file.path(load_os_leading_dir(), "Dropbox/IGME Data/Output CMRJack/All/BH", surveytype, "Real/Optimal")
  files <- get.file.name(dir_file =dir_opt,  pattern0 = cname)
  if(any(grepl(" CY ", files))) files <- grep(" CY ", files, value = TRUE)
  if(!is.null(year))files <- grep(year, files, value = TRUE)
  return(files)
}

#' Get raw file directory from `Output CMRJack` folder
#' @param cname country name
#' @param surveytype folder names like "DHS", "MICS", "NDHS",...
#' @param year year of the survey, e.g. 2015
#' @return xlsx file directory
#' @export get.raw.dir
#' @examples
#' \dontrun{
#' get.raw.dir("Zimbabwe", "DHS", 2015)
#' }
get.raw.dir <- function(cname, surveytype = "DHS", year = NULL){
  cname <- gsub(" ", "", cname)
  dir_opt <- file.path(load_os_leading_dir(), "Dropbox/IGME Data/Output CMRJack/All/BH", surveytype, "Real/Raw")
  files <- get.file.name(dir_file =dir_opt,  pattern0 = cname)
  if(any(grepl(" CY ", files))) files <- grep(" CY ", files, value = TRUE)
  if(!is.null(year)) files <- grep(year, files, value = TRUE)
  return(files)
}

# extra

#' Adjust the file dir if the lash is not right or the Dropbox username is not
#' right
#'
#' @param dir0 file directory not output for now
#' @export revise.path
revise.path <- function(dir0){
  # if there is backslack, replace it
  if(grep("\\\\", dir0)) dir <- gsub("\\\\", "\\/", dir0)
  # replace username if it is not right
  if(!grepl(Sys.getenv("USERNAME"), dir)) dir <- file.path(load_os_leading_dir(),"Dropbox", sub("^.*Dropbox", "", dir))
  if(!file.exists(dir)) stop("check if dir exists: ", dir)
  return(dir)
}

#' check system, return . Used as nternal function.
#'
#' @return "Windows", "OSX", or "Linus"
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}


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
#'
load_os_leading_dir <- function(){
  user <- Sys.info()[["user"]]
  os <- get_os()
  if(!os %in% c("windows", "osx")) warning ("Have only saved directories for Windows and Mac OSX")
  leading_path <- if(os == "osx") file.path("/Users", user) else Sys.getenv("USERPROFILE")
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
                              "/Dropbox/IGME 5-14/2020 Round Estimation/Aggregate results (final) 2020-08-31/Rates & Deaths_Country Summary.csv"),
    dir_15_24_2020 = file.path(leading_path,
                               "/Dropbox/IGME 15-24/2020 Round Estimation/Aggregate results (final) 2020-08-31/Rates & Deaths_Country Summary.csv")
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

