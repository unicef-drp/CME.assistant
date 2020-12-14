# functions to search and get all major datasets
# functions to add new entries to master datasets for U5MR, IMR, NMR

#'Get IGME "Code" dir for a given year
#'
#'If `year` is 2020, returns the directory to Code folder in the 2020 Round
#'Estimation Dropbox folder
#'@param year YYYY
#'@return directory to input folder
#'@export get.IGME.dir
get.IGME.dir <- function(year){
  USERPROFILE <- Sys.getenv("USERPROFILE")
  file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", year ," Round Estimation/Code/"))
}


#' Load the IGME "input" directories
#'
#' @return dir_IGMEinput_list: a list of directories of UN IGME Data/YYYY Round
#'   Estimations/Code/input
#' @export load.IGMEinput.dir
load.IGMEinput.dir <- function(){
  # the input folder:
  USERPROFILE <- Sys.getenv("USERPROFILE")
  dir_IGMEinput_list <- list(
    dir_IGME_thisyear = file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", format(Sys.Date(), "%Y") ," Round Estimation/Code/input/")),
    dir_IGME_21       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2021 Round Estimation/Code/input/"),
    dir_IGME_20       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2020 Round Estimation/Code/input/"),
    dir_IGME_19       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2019 Round Estimation/Code/input/"),
    dir_IGME_NMR      = file.path(USERPROFILE, "/Dropbox/NMR/data")
  )
  return(dir_IGMEinput_list)
}


#'Get "input" dir for a given year
#'
#'If `year` is 2020, returns the directory to input folder in the 2020 Round
#'Estimation Dropbox folder
#'@param year YYYY
#'@return directory to input folder
#'@export get.IGMEinput.dir
get.IGMEinput.dir <- function(year){
  USERPROFILE <- Sys.getenv("USERPROFILE")
  file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", year ," Round Estimation/Code/input/"))
}

#' load the IGME "output" directories
#' @return dir_IGMEoutput_list: a list of directories to UN IGME Data/YYYY Round
#'   Estimations/Code/output
#' @export load.IGMEoutput.dir
load.IGMEoutput.dir <- function(){
  USERPROFILE <- Sys.getenv("USERPROFILE")
  dir_IGMEoutput_list <- list(
    dir_IGME_thisyear = file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", format(Sys.Date(), "%Y") ," Round Estimation/Code/output/")),
    dir_IGME_21       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2021 Round Estimation/Code/output/"),
    dir_IGME_20       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2020 Round Estimation/Code/output/"),
    dir_IGME_19       = file.path(USERPROFILE, "/Dropbox/UN IGME Data/2019 Round Estimation/Code/output/"),
    dir_IGME_NMR      = file.path(USERPROFILE, "/Dropbox/NMR/output")
  )
  return(dir_IGMEoutput_list)
}

#'Get "output" dir for a given year
#'
#'If `year` is 2020, returns the directory to output folder in the 2020 Round
#'Estimation Dropbox folder
#'#'
#' @param year YYYY
#' @return directory to output folder
#' @export get.IGMEoutput.dir
get.IGMEoutput.dir <- function(year){
  USERPROFILE <- Sys.getenv("USERPROFILE")
  file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", year ," Round Estimation/Code/output/"))
}

#'Get "fig" dir for a given year
#'
#'If `year` is 2020, returns the directory to fig folder in the 2020 Round
#'Estimation Dropbox folder
#'#'
#' @param year YYYY
#' @return directory to fig folder
#' @export get.IGMEfig.dir
get.IGMEfig.dir <- function(year){
  USERPROFILE <- Sys.getenv("USERPROFILE")
  file.path(USERPROFILE, paste0("/Dropbox/UN IGME Data/", year ," Round Estimation/Code/fig/"))
}


#' Calculate start, end and average date in decimal from starting/end dates
#' @importFrom lubridate leap_year
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
  get.date <- function(date0){
    y1 <- data.table::year(date0)
    n_days1 <- ifelse(lubridate::leap_year(y1), 366, 365) # e.g. 2020 is a leap year with 366 days
    first_day_of_year <- as.Date(paste(y1, 1, 1, sep = "-")) # use to count diff days
    date_num <- as.double(difftime(date0, first_day_of_year))/n_days1 + y1
    return(date_num)
  }
  date_start <- get.date(date0)
  date_end <- get.date(date1)
  date_ave <- get.date(date0 + difftime(date1, date0)/2)
  return(list(date_start=date_start, date_end=date_end, date_ave=date_ave))
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

  if(is.null(dir_file))stop("dir_file is NULL. Please double check.")
  if(!dir.exists(dir_file))stop("Check if dir_file exists: ", dir_file)
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
#' @importFrom stringr str_remove_all
find_latest_date <- function(files){
  remove_string <- c("data_U5MR_|.csv|data_IMR_|data_NMR_|_5year|dataset_formodeling_|dataset_forplotting_")
  dates <- stringr::str_remove_all(string = files, pattern = remove_string)
  # screen for valid date string:
  # dates <- c("2015", "20200804", "2020-08-01")
  # return which.max e.g. 2L
  get.max.date(dates)
}


#' Get the U5MR master dataset directory
#'
#' @param dir_IGME The directory to IGME input folder, e.g. ".../2020 Round
#'   Estimation/Code/input/", could be obtained using
#'   \code{\link{get.IGMEinput.dir}}
#' @return file path to the master dataset
#' @export get.dir_U5MR
get.dir_U5MR <- function(dir_IGME = get.IGMEinput.dir(2020)){
  files_full <- get.file.name(dir_file = dir_IGME, pattern0 = "data_U5MR")
  files <- get.file.name(dir_file = dir_IGME, pattern0 = "data_U5MR", full_name = FALSE)
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
get.dir_IMR <- function(dir_IGME = get.IGMEinput.dir(2020)){
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
#' @return file path to the master dataset
#' @export get.dir_NMR
get.dir_NMR <- function(
  y5 = FALSE
  ){
  dir_IGME_NMR <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/NMR/data")
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
#' @param plotting to get the dataset for plotting (if TRUE) or dataset for
#'   modeling (if FALSE)
#' @return file path to the master dataset
#' @export get.dir_gender
get.dir_gender <- function(
  plotting = TRUE
  ){
  dir_IGME_gender <- file.path(Sys.getenv("USERPROFILE"),"/Dropbox/CMEgender2015/Database")
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
  dir_opt <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/IGME Data/Output CMRJack/All/BH", surveytype, "Real/Optimal")
  files <- get.file.name(dir_file =dir_opt,  pattern0 = cname)
  if(any(grepl(" CY ", files))) files <- grep(" CY ", files, value = TRUE)
  if(!is.null(year))files <- grep(year, files, value = TRUE)
  return(files)
}

#' get raw file directory from `Output CMRJack` folder
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
  dir_opt <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/IGME Data/Output CMRJack/All/BH", surveytype, "Real/Raw")
  files <- get.file.name(dir_file =dir_opt,  pattern0 = cname)
  if(any(grepl(" CY ", files))) files <- grep(" CY ", files, value = TRUE)
  if(!is.null(year)) files <- grep(year, files, value = TRUE)
  return(files)
}

# extra

#' Adjust the file dir if the lash is not right or the dropbox username is not
#' right (YL 2020/2)
#'
#' @param dir0 file directory not output for now
revise.path <- function(dir0){
  # if there is backslack, replace it
  if(grep("\\\\", dir0)) dir <- gsub("\\\\", "\\/", dir0)
  # replace username if it is not right
  if(!grepl(Sys.getenv("USERNAME"), dir)) dir <- file.path(Sys.getenv("USERPROFILE"),"Dropbox", sub("^.*Dropbox", "", dir))
  if(!file.exists(dir)) stop("check if dir exists: ", dir)
  return(dir)
}

#' Create IGME_Key column
#'
#' Extra strings like "Preliminary" or "MM/NN adjusted" are removed in the
#' created `IGME_Key` column
#'
#' @param dt0 dataset
#'
#' @return dt0 dataset with added column `IGME_Key`
#' @export create.IGME.key
create.IGME.key <- function(dt0){
  strings_to_remove <- " \\(Adjusted\\)| \\(MM adjusted\\)| \\(NN adjusted\\)| \\(Preliminary\\)| \\(preliminary\\)"

  # the process to create IGME_Key
  if ("Country.Code"%in%colnames(dt0)&is.character(dt0$Country.Code)) {
    dt0[, Code:= Country.Code]
  } else if ("Country.ISO"%in%colnames(dt0)&is.character(dt0$Country.ISO)) {
    dt0[, Code:= Country.ISO]
  } else {stop("Check Country.Code and Country.ISO")}
  # Some SVR like South Africa has year associated with it
  dt0[Series.Category %in% c("VR", "SVR"), IGME_Key := paste0(Code, "-", Series.Category)]
  dt0[Series.Type %in% c("Life Table"), IGME_Key := paste0(Code, "-", Series.Type)]
  # dt0[Series.Type %in% c("Life Table"), ]
  dt0[!Series.Category %in% c("VR", "SVR", "Life Table"), IGME_Key := paste0(Code, "-", Series.Year, "-", Series.Name)]
  dt0[Series.Category %in% c("SVR") & Country.Name == "South Africa", IGME_Key := paste0(Code, "-", Series.Year, "-", Series.Category)]
  dt0[, IGME_Key := gsub(strings_to_remove, "", IGME_Key)]
  dt0[, Code:=NULL]
  return(dt0)
}


# Add new series ----------------------------------------------------------

#' Row-bind two datasets, check duplicated keys and set order
#'
#' @param dt_master master dataset
#' @param dt_new new entries
#' @return `dt1` as `rbind(dt_master, dt_new)`
#' @export rbinddataset
rbinddataset <- function(dt_master, dt_new){
  message("old nrow:", nrow(dt_master))
  dt1 <- rbind(dt_master, dt_new)
  dup_key <- dt1[duplicated(dt1), unique(IGME_Key)]
  if(length(dup_key)>0) message("Notice duplicated series: ", paste(dup_key, collapse = ", "))
  setorder(dt1, Country.Name, -Indicator, -Sex, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, - Inclusion)
  message("new nrow:", nrow(dt1))
  if(nrow(dt_master) + nrow(dt_new) != nrow(dt1)) warning("check row numbers ")
  return(dt1)
}

#' Row-bind two datasets for NMR with slight changes, check duplicated keys, set
#' order
#'
#' @param dt_master master dataset
#' @param dt_new new entries
#' @return `dt1` as `rbind(dt_master, dt_new)`
#' @export rbinddatasetNMR
rbinddatasetNMR <- function(dt_master, dt_new){
  message("old nrow:", nrow(dt_master))
  dt1 <- rbind(dt_master, dt_new)
  dup_key <- dt1[duplicated(dt1), unique(IGME_Key)]
  if(length(dup_key)>0) message("Notice duplicated series: ", paste(dup_key, collapse = ", "))
  setorder(dt1, Country.Name, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, -Inclusion)
  message("new nrow:", nrow(dt1))
  if(nrow(dt_master) + nrow(dt_new) != nrow(dt1)) warning("check row numbers ")
  return(dt1)
}

#' Remove 25-34 age group in `dt_new_entries`
#'
#' Internal function used by `add.new.series` functions
#' @param dt_new_entries dt of new entries to be added
revise.age.group <- function(dt_new_entries){
  if(nrow(dt_new_entries[Age.Group.of.Women=="25-34"])>0){
    message("Remove AOW group '25-34' for ", paste(dt_new_entries[Age.Group.of.Women=="25-34", unique(IGME_Key)], collapse = ", "))
    dt_new_entries <- dt_new_entries[Age.Group.of.Women!="25-34", ]
  }
  return(dt_new_entries)
}

#' Add an original series name
#'
#' Add `Original.Series.Name` column by the supplied `ori_name` argument
#'
#' @param dt_new_entries  dt of new entries to be added
#' @param ori_name value for Original.Series.Name
#' @return dt_new_entries
#' @export add.Original.Series.Name
add.Original.Series.Name <- function(dt_new_entries, ori_name){
  dt_new_entries[, Original.Series.Name:= as.character(Original.Series.Name)]
  dt_new_entries[, Original.Series.Name:= ori_name]
  return(dt_new_entries)
}

#' Add new U5MR or IMR entries (dt_new_entries)
#'
#' @param dt_master the U5MR master dataset
#' @param dt_new_entries  dt of new entries to be added
#' @param remove_old if TRUE, remove old entries, if FALSE, set old entries to invisible and excluded
#' @return the new dt_master
#' @export add.new.series.u5mr
add.new.series.u5mr <- function(
  dt_master,
  dt_new_entries,
  remove_old = TRUE
  ){
  message("original nrow:", nrow(dt_master))
  dt_new_entries <- revise.age.group(dt_new_entries)
  nrow_old <- nrow(dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key),])
  if(nrow_old > 0){
    if(remove_old){
      message("Remove ", nrow_old, " existing (perhaps old) entries: ",
              paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
      dt_master <- dt_master[!IGME_Key %in% unique(dt_new_entries$IGME_Key)]
      message("nrow after removing old entries:", nrow(dt_master))

    } else {
      message("Change inclusion and visible for old entry to 0: ", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
      dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key), Inclusion:=0]
      dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key), Visible := 0]
    }

  }
  # recreate IGME Key
  dt_new_entries <- create.IGME.key(dt_new_entries)
  dt1 <- rbind(dt_master, dt_new_entries)
  dup_key <- dt1[duplicated(dt1), unique(IGME_Key)]
  if(length(dup_key)>0) message("Notice duplicated series: ", paste(dup_key, collapse = ", "))
  setorder(dt1, Country.Name, -Indicator, -Sex, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, - Inclusion)
  message("new nrow:", nrow(dt1), " -adding- ", nrow(dt1) - nrow(dt_master))
  message("Newly added:", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
  if(nrow(dt_master) + nrow(dt_new_entries) != nrow(dt1)) warning("check row numbers ")
  return(dt1)
}

#' Add new IMR entries (dt_new_entries), now the same function as
#' `add.new.series.u5mr`
#'
#' Still kept in case needed in the future in case need to differentiate IMR
#' from U5MR process
#'
#' @param dt_IMR the IMR master dataset
#' @param dt_new_entries  dt of new entries to be added
#' @param remove_old if TRUE, remove old entries, if FALSE, set old entries to
#'   invisible and excluded
#' @return the new dt_IMR
#' @export add.new.series.imr
add.new.series.imr <- function(
  dt_IMR,
  dt_new_entries,
  remove_old = TRUE
  ){
  message("original nrow:", nrow(dt_IMR))
  dt_new_entries <- revise.age.group(dt_new_entries)
  # dt_new_entries$To.be.adjusted
  # dt_IMR[, table(To.be.adjusted, useNA = "ifany")]
  # dt_IMR$To.be.adjusted <- NA
  if(nrow(dt_IMR[IGME_Key %in% unique(dt_new_entries$IGME_Key),]) > 0){
    if(remove_old){
      message("Remove existing (possibly old) entries: ", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
      dt_IMR <- dt_IMR[!IGME_Key %in% unique(dt_new_entries$IGME_Key)]
    } else {
      message("Change inclusion and visible for old entry to 0: ", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
      dt_IMR[IGME_Key %in% unique(dt_new_entries$IGME_Key), Inclusion:=0]
      dt_IMR[IGME_Key %in% unique(dt_new_entries$IGME_Key), Visible := 0]
    }

  }
  message("nrow after removing old entries:", nrow(dt_IMR))
  # recreate IGME Key
  dt_new_entries <- create.IGME.key(dt_new_entries)
  dt1 <- rbind(dt_IMR, dt_new_entries)
  dt1[duplicated(dt1), unique(IGME_Key)]
  setorder(dt1, Country.Name, -Indicator, -Sex, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, - Inclusion)
  message("new nrow:", nrow(dt1), " -adding- ", nrow(dt1) - nrow(dt_IMR))
  message("Newly added:", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
  if(nrow(dt_IMR) + nrow(dt_new_entries) != nrow(dt1)) warning("check row numbers ")
  return(dt1)
}

#' Add new NMR entries (`dt_new_entries`)
#'
#' @param dt_nmr the NMR master dataset
#' @param dt_new_entries dt of new entries to be added
#' @return the new dt_nmr_new
#' @export add.new.series.nmr
add.new.series.nmr <- function(
  dt_nmr,
  dt_new_entries
  ){
  message("old nrow:", nrow(dt_nmr))

  key0s <- dt_new_entries[, unique(IGME_Key)]
  dt_existing <- dt_nmr[IGME_Key%in%key0s & Visible==1]
  if(nrow(dt_existing)>0){
    message("Remove ", dt_existing[,.N], " rows of ", dt_existing$IGME_Key[1] ," as old data exist")
    dt_nmr <- dt_nmr[!(IGME_Key%in%key0s & Visible==1)]
  }

  for(key0 in key0s){
    dt_nmr[IGME_Key==key0]
    if(nrow(dt_nmr[IGME_Key==key0])>0) {
      message("Check existing entries for ", key0)
      message("Inclusion for", key0, dt_nmr[IGME_Key==key0, Inclusion], "\n")
      dt_nmr[IGME_Key==key0, Inclusion:=0]
      message("New Inclusion for", key0, dt_nmr[IGME_Key==key0, Inclusion], "\n")
      message("Visible for", key0, dt_nmr[IGME_Key==key0, Visible], "\n")
      dt_nmr[IGME_Key==key0, Visible:=0]
      message("New Visible for", key0, dt_nmr[IGME_Key==key0, Visible], "\n")
    }
  }
  # recreate IGME Key
  dt_new_entries <- create.IGME.key(dt_new_entries)
  dt_nmr_new <- rbind(dt_nmr, dt_new_entries, fill = TRUE)
  if(nrow(dt_nmr) + nrow(dt_new_entries) != nrow(dt_nmr_new)) warning("check row numbers ")
  if(ncol(dt_nmr) != ncol(dt_new_entries)) warning("check col numbers ")
  setorder(dt_nmr_new, Country.Name, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, -Inclusion)

  message("new nrow:", nrow(dt_nmr_new), " -adding- ", nrow(dt_nmr_new) - nrow(dt_nmr))

  message("old ncol:", ncol(dt_nmr))
  message("new ncol:", ncol(dt_nmr_new))
  message("Newly added:", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))

  return(dt_nmr_new)
}


