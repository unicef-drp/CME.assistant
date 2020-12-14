# Functions for making IGME CC profile
# 2020/08

# Read Data ---------------------------------------------------------------

#' Read one results.csv file and reformat into long-format
#'
#' @importFrom readxl read_xlsx
#' @param dt_dir the single directory to a results.csv file
#' @param year_range year range desired, default to `1931:2019`
#' @param q quantile desired, default to `c("Lower", "Median", "Upper")`
#' @param sex default to NULL, Sex is determined from `dt_dir`, unless specified
#' @return long-format dt containing all indicators
#' @export read.results.csv.file
read.results.csv.file <- function(
  dt_dir,
  year_range = 1931:2019,
  q = c("Lower", "Median", "Upper"),
  sex = NULL
  ){
  id_vars <- c("ISO.Code", "Quantile", "Indicator")
  if(grepl(".xlsx", dt_dir)){
    dt1 <- setDT(readxl::read_xlsx(dt_dir))
  } else {
    dt1 <- fread(dt_dir, header = TRUE)
  }
  dt1 <- dt1[Quantile %in% q, ]
  # align the column names first
  if("ISO Code"%in%colnames(dt1)) setnames(dt1, "ISO Code", "ISO.Code")
  if(any(grepl("X", colnames(dt1)))){
    value_vars <- paste0("X", year_range, ".5")
  } else if (any(grepl(".5", colnames(dt1), fixed = TRUE))) {
    value_vars <- paste0(year_range, ".5")
  } else {
    value_vars <- paste0(year_range)
  }
  value_vars <- value_vars[value_vars%in%colnames(dt1)]
  dt1[, (value_vars):= lapply(.SD, as.numeric), .SDcols = value_vars]
  if(grepl("U5MR", dt1$Indicator[1])) dt1$Indicator <- "Under-five Mortality Rate"
  if(grepl("IMR", dt1$Indicator[1])) dt1$Indicator <- "Infant Mortality Rate"
  if(grepl("CMR", dt1$Indicator[1])) dt1$Indicator <- "Child mortality rate age 1-4"
  if(grepl("10q5", dt_dir))dt1$Indicator <- "Mortality for 5-14 year-olds"
  if(grepl("10q15", dt_dir))dt1$Indicator <- "Mortality for 15-24 year-olds"
  if(grepl("5q5", dt_dir))dt1$Indicator <- "Mortality for 5-9 year-olds"
  if(grepl("5q15", dt_dir))dt1$Indicator <- "Mortality for 15-19 year-olds"
  # message("value_vars is ", paste(value_vars[1:5], collapse = ", "), "...")
  # message("colnames is ", paste(colnames(dt1)[1:10], collapse = ", "), "...")
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
    if(grepl("female", dt_dir)){
      sex <- "Female"
    } else if (grepl("male", dt_dir)) {
      sex <- "Male"
    } else {
      sex <- "Total"
    }
  }
  dt1_long[, Sex:= sex]
  setkey(dt1_long, ISO.Code, Year)
  return(dt1_long)
}

#' Collect data from the cqt file for plotting
#'
#' `get.dt.cqt` loads all cqt files in the folder as one long-format dataset
#'
#' @param cqt_dir where all the `res.cqt.rds` files are saved
#' @return dt_cqt
#' @export get.dt.cqt
get.dt.cqt <- function(cqt_dir){
  cqt_file_list <- list.files(cqt_dir, full.names = TRUE)
  cqt_file_list <- cqt_file_list[!grepl("Ratio",cqt_file_list)]
  cqt_file_list <- cqt_file_list[!grepl("CMR",cqt_file_list)]
  read.cqt.backup <- function(file){
    dt_cqt <- setDT(readRDS(file))
    dt_cqt[, Quantile:=as.factor(Quantile)]
    levels(dt_cqt$Quantile) <- c("Lower", "Median", "Upper")
    dt_cqt <- na.omit(dt_cqt)
    dt_cqt[, ISO.Code:=as.character(ISO.Code)]
    setkey(dt_cqt, ind_short, Sex, ISO.Code, Quantile, Year)
    return(dt_cqt)
  }
  dt_cqt <- rbindlist(lapply(cqt_file_list, read.cqt.backup))
  dt_cqt[, Year:=floor(Year)]
  dt_cqt <- dt_cqt[Year%in%1990:2019]
  setnames(dt_cqt, "ind_short", "Shortind")
  setkey(dt_cqt, Shortind, Sex, ISO.Code, Quantile, Year)
  return(dt_cqt)
}

#' Get CC profile data by country
#'
#' Get the table that fits into the CC profile excel template. Fit the format of
#' the template with blank rows between indicators. Follow the order of median,
#' lower and upper. Only the values (not row/col names) are put into the
#' template later
#'
#' @param iso0 ISO3Code
#' @param ind_wanted indicators used in the profile, default to `c("10q15",
#'   "10q5", "IMR", "NMR", "U5MR")`
#' @return formatted table by country
#' @export get.table.by.iso
get.table.by.iso <- function(
  iso0,
  ind_wanted = c("10q15", "10q5", "IMR", "NMR", "U5MR")
  ){
  dt_cqt0 <- dt_cqt[ISO.Code==iso0]
  # for inserting blank rows beneath each indicator
  dt_cqt_blank <- dt_cqt0[Quantile=="Median"]
  dt_cqt_blank[, `:=`(Value = NA, Quantile = "Z_Blank")]
  dt_cqt0 <- rbindlist(list(dt_cqt0, dt_cqt_blank))
  dt_cqt0 <- dt_cqt0[Shortind%in%ind_wanted]
  dt_cqt0[, Shortind := as.factor(Shortind)]
  if(!identical(levels(dt_cqt0$Shortind),c("10q15", "10q5",  "IMR",   "NMR",   "U5MR"))) stop("Check Shortind levels")
  levels(dt_cqt0$Shortind) <- c("5.10q15", "4.10q5",  "2.IMR",   "3.NMR",   "1.U5MR")
  dt_cqt0$Sex <- as.factor(dt_cqt0$Sex)
  if(!identical(levels(dt_cqt0$Sex),c("Female", "Male",  "Total"))) stop("Check Sex levels")
  levels(dt_cqt0$Sex) <- c("3.Female", "2.Male", "1.Total")

  dt_cqt0$Quantile <- as.factor(dt_cqt0$Quantile)
  if(!identical(levels(dt_cqt0$Quantile),c("Lower", "Median",  "Upper", "Z_Blank"))) stop("Check Quantile levels")
  levels(dt_cqt0$Quantile) <- c("2.Lower",  "1.Median", "3.Upper", "4.Blank")
  dt_cqt0[, ID:= paste(Shortind, Sex, Quantile)]
  dt_cqt0[, Value := roundoff(Value, 1)]
  dt_cqt0_wide <- dcast(dt_cqt0, ID~Year, value.var = "Value")
  setcolorder(dt_cqt0_wide, c("ID", 2019:1990))
  return(dt_cqt0_wide)
}


# Source ------------------------------------------------------------------

#' Format the legend_ID for the master datasets
#'
#' Used in \code{\link{get.0_4.sources}} and \code{\link{get.5_24.sources}} to
#' create sources for template input
#'
#' @param data datasets
#'
#' @return subsetted dataset with revised Series.Name and new column `legend_ID`
#' @export get.new.sourceID
get.new.sourceID <- function(data = dt_cme){
  strings_to_remove <- " \\(Preliminary\\)| \\(preliminary\\)| 0"
  data[, Series.Name := gsub(strings_to_remove, "", Series.Name)]
  # create legend_ID depends on if it is VR series:
  if(data[Series.Type == "VR",.N]>0){
    data[Series.Type == "VR", VR_range:= paste(range(floor(Reference.Date)), collapse = "-"), by = Country.Code]
    data[Series.Type == "VR", legend_ID := VR_range]
  }
  if(data[Series.Type != "VR",.N]>0){
    data[Series.Type != "VR", legend_ID := paste(Series.Name, Series.Year)]
  }
  data[, legend_ID := gsub(strings_to_remove, " ", legend_ID)] # an extra space
  setorder(data, Country.Name, End.date.of.Survey, Series.Name, Series.Type, Reference.Date)
  return(data[,.(Country.Code, Country.Name, Start.date.of.Survey, End.date.of.Survey, Series.Name, Series.Type, Date.Of.Data.Added, Reference.Date, Inclusion, legend_ID)])
}

#' Get survey names for Table Under 5
#'
#' @param dt_U5MR U5MR master dataset
#' @param dt_IMR IMR
#' @param dt_NMR NMR
#' @return list of 2 datasets: surveys sources and VR sources
#' @export get.0_4.sources
get.0_4.sources <- function(dt_U5MR, dt_IMR, dt_NMR){
  # prepare datasets
  dt1 <- get.new.sourceID(dt_U5MR)
  dt1$ind <- "U5MR"
  dt2 <- get.new.sourceID(dt_IMR)
  dt2$ind <- "IMR"
  dt3 <- get.new.sourceID(dt_NMR)
  dt3$ind<- "NMR"
  # combine datasets
  dtall <- unique(rbindlist(list(dt1, dt2, dt3)))
  dtall[, inds := paste(unique(ind), collapse = "/"), by = .(Country.Code, legend_ID)]
  # Survey
  dtall_survey <- unique(dtall[Series.Type != "VR",.(Country.Code, Start.date.of.Survey, End.date.of.Survey, Country.Name, legend_ID, inds)])
  dtall_survey[, surveys:= paste0(legend_ID, " (", inds, ")"), by = Country.Code]
  setorder(dtall_survey, Country.Name, Start.date.of.Survey, End.date.of.Survey)
  dtall_survey[, surveys_combined := paste(unique(surveys), collapse = "; "), by = Country.Code]
  dtall_survey <- unique(dtall_survey[,.(Country.Code, surveys_combined)])
  # VR
  dtall[ind=="U5MR"&Series.Type == "VR", `:=`(
    VR_range= paste(range(floor(Reference.Date)), collapse = "-"),
    year1 = range(floor(Reference.Date))[1],
    year2 = range(floor(Reference.Date))[2]), by = Country.Code]
  dtall[year1==year2, VR_range := year1] # in case same year

  dtall_VR <- na.omit(unique(dtall[,.(Country.Code, Country.Name, VR_range)]))
  return(list(survey = dtall_survey, vr = dtall_VR))
}

#' Get survey names (sources) for 5 to 24
#'
#' @param dt5_14 dt 5-14
#' @param dt15_24 dt 15-24
#' @return list of 3 datasets: sources for 5-14, for 15-24 and vr
#' @export get.5_24.sources
get.5_24.sources <- function(dt5_14, dt15_24){
  get.youth.survey <- function(dt_cme, new_var = "source5_14"){
    dt_cme <- dt_cme[Visible==1]
    dt_cme <- dt_cme[!Series.Name%like%"Derived from 5q0"][!Data.Source%like%"Derived from 5q0"][Series.Type != "VR"]
    dt4 <- get.new.sourceID(dt_cme) # elements like 0 shall be removed in this step
    setorder(dt4,  Country.Name, Start.date.of.Survey, End.date.of.Survey)
    dt4ss <- dt4[, surveys_combined := paste(unique(legend_ID), collapse = "; "), by = Country.Code]
    dt4ss <- unique(dt4ss[Series.Type != "VR",.(Country.Name, Country.Code, surveys_combined)])
    setnames(dt4ss, "surveys_combined", new_var)
    setkey(dt4ss, Country.Code, Country.Name)
    dt4ss
  }
  dt4ss <- get.youth.survey(dt5_14, new_var = "source5_14")
  dt5ss <- get.youth.survey(dt15_24, new_var = "source15_24")

  # VR
  dtall <- unique(rbindlist(list(dt5_14, dt15_24), fill = TRUE))
  dtall <- dtall[Visible==1]
  dtall <- dtall[!Series.Name%like%"Derived from 5q0"][!Data.Source%like%"Derived from 5q0"][Series.Type == "VR"]
  dtall <- get.new.sourceID(dtall)
  dtall[, `:=`(
    VR_range= paste(range(floor(Reference.Date)), collapse = "-"),
    year1 = range(floor(Reference.Date))[1],
    year2 = range(floor(Reference.Date))[2]), by = Country.Code]
  dtall[year1==year2, VR_range := year1] # in case same year
  dtall_VR <- na.omit(unique(dtall[,.(Country.Code, Country.Name, VR_range)]))
  setkey(dtall_VR, Country.Code)

  return(list(survey1 = dt4ss, survey2 = dt5ss, vr = dtall_VR))
}


# Save Excel -------------------------------------------------------------------

#' Save CMR CC profile by country
#'
#' `save.CME.CC.profile` writes excel template using the `XLSX` package
#' @import xlsx
#'
#' @param iso0 ISO3Code
#' @param template directory to template, default to project root folder
#' @param dir_save directory to save output, default to folder "AfterCC",
#'   folders are created if doesn't exist
#' @export save.CME.CC.profile
#' @return NULL, save xlsx in `dir_save`
save.CME.CC.profile <- function(iso0,
                                template = "IGME CC Template.xlsx",
                                dir_save = "AfterCC"){

  addString<-function(sheet, rowIndex, colIndex, string, stringStyle){
    rows <-createRow(sheet,rowIndex=rowIndex)
    sheetTitle <-createCell(rows, colIndex)
    setCellValue(sheetTitle[[1,1]], string)
    setCellStyle(sheetTitle[[1,1]], stringStyle)
  }

  get.fig.dir <- function(pattern0, iso0){
    dir0 <- fig_dirs[fig_dirs%like%pattern0 & fig_dirs%like%iso0]
    if(length(dir0)==0) stop("Check your pattern input, couldn't match fig dir.")
    return(dir0)
  }

  cname0 <- dt_new_cnames[ISO3Code==iso0, OfficialName]
  wb = loadWorkbook(template)
  sheet = getSheets(wb)

  # some text styles
  TITLE_STYLE1 <- CellStyle(wb)+ Font(wb, name = "Arial", heightInPoints=14, isBold=TRUE) + Alignment(horizontal="ALIGN_CENTER", vertical="VERTICAL_CENTER") # title
  TITLE_STYLE2 <- CellStyle(wb)+ Font(wb, name = "Arial", heightInPoints=18, isBold=TRUE) + Alignment(horizontal="ALIGN_CENTER", vertical="VERTICAL_CENTER") # title --- larger for the plot sheets
  # source
  TEXT_STYLE1 <- CellStyle(wb) + Font(wb, name = "Arial", heightInPoints=9, isBold = FALSE) + Alignment(vertical="VERTICAL_TOP", wrapText = TRUE)
  # data
  DATA_STYLE <-  CellStyle(wb) + Font(wb, name = "Arial", heightInPoints = 9) +
    DataFormat("0.0") + Alignment(horizontal="ALIGN_RIGHT", vertical="VERTICAL_CENTER")

  # Add title
  add.title <- function(sheet0){
    addString(sheet0, rowIndex = 4, colIndex = 14,
              string = cname0, stringStyle = TITLE_STYLE1)}
  invisible(lapply(sheet[1:2], add.title)) # for sheet 1 and 2
  add.title <- function(sheet0){
    addString(sheet0, rowIndex = 4, colIndex = 13,
              string = cname0, stringStyle = TITLE_STYLE2)}
  invisible(lapply(sheet[3:4], add.title)) # for sheet 3 and 4

  # sheet 1 - Under-5 ----
  sheet1 = sheet[[1]]
  # Add sources
  source_row = 44
  source0 <- unlist(CME_sources$vr[Country.Code==iso0, VR_range])
  source1 <- unlist(CME_sources$survey[Country.Code==iso0, surveys_combined])
  if(length(source0)==0) source0 <- "Not available"
  if(length(source1)==0) source1 <- "Not available"

  add.source.and.format <- function(source0, sheet0, source_row0){
    addDataFrame(x = source0, sheet = sheet0,
                 startRow = source_row0, startColumn  = 3,
                 row.names = F, col.names = F)
    source_cell <- getCells(getRows(sheet0, rowIndex = source_row0), colIndex = 3)
    setCellStyle(source_cell[[1]], TEXT_STYLE1)
  }
  add.source.and.format(source0 = source0, sheet0 = sheet1, source_row0 = source_row)
  add.source.and.format(source0 = source1, sheet0 = sheet1, source_row0 = source_row+2)
  #
  # Add data of under-5
  CME_data_table <- get.table.by.iso(iso0 = iso0)
  # U5MR on row 10 --- C10
  addDataFrame(x = CME_data_table[1:27, 2:ncol(CME_data_table)],
               sheet1, startRow = 10, startColumn  = 3,
               row.names = F, col.names = F)
  data_rows <- getRows(sheet1, rowIndex = 10:36)
  data_cells <- getCells(data_rows, colIndex = 3:(ncol(CME_data_table)-1+2))
  invisible(lapply(data_cells, setCellStyle, DATA_STYLE))

  # Sheet 2 - 5-24 ----
  sheet2 = sheet[[2]]
  source0 <- unlist(CME_sources5_24$vr[Country.Code==iso0, VR_range])
  source1 <- unlist(CME_sources5_24$survey1[Country.Code==iso0, source5_14])
  source2 <- unlist(CME_sources5_24$survey2[Country.Code==iso0, source15_24])
  if(length(source0)==0) source0 <- "Not available"
  if(length(source1)==0) source1 <- "Not available"
  if(length(source2)==0) source2 <- "Not available"
  add.source.and.format(source0 = source0, sheet0 = sheet2, source_row0 = 25)
  add.source.and.format(source0 = source1, sheet0 = sheet2, source_row0 = 27)
  add.source.and.format(source0 = source2, sheet0 = sheet2, source_row0 = 28)
  #
  # Add data of 5-24
  # start on row 10 --- C10
  addDataFrame(x = CME_data_table[29:35, 2:ncol(CME_data_table)],
               sheet2, startRow = 10, startColumn  = 3,
               row.names = F, col.names = F)
  data_rows <- getRows(sheet2, rowIndex = 10:16)
  data_cells <- getCells(data_rows, colIndex = 3:(ncol(CME_data_table)-1+2))
  invisible(lapply(data_cells, setCellStyle, DATA_STYLE))

  # Sheet 3 - Graph under 5----
  sheet3 <- sheet[[3]]
  # U5MR Total
  scale0 = 0.77
  addPicture(get.fig.dir("U5MR_Total", iso0 = iso0),
             sheet3, startRow = 8, startColumn = 1, scale = scale0)
  addPicture(get.fig.dir("U5MR_Male", iso0 = iso0),
             sheet3, startRow = 45, startColumn = 1, scale = scale0)
  addPicture(get.fig.dir("U5MR_Female", iso0 = iso0),
             sheet3, startRow = 82, startColumn = 1, scale = scale0)
  addPicture(get.fig.dir("IMR_Total", iso0 = iso0),
             sheet3, startRow = 119, startColumn = 1, scale = scale0)
  addPicture(get.fig.dir("IMR_Male", iso0 = iso0),
             sheet3, startRow = 156, startColumn = 1, scale = scale0)
  addPicture(get.fig.dir("IMR_Female", iso0 = iso0),
             sheet3, startRow = 193, startColumn = 1, scale = scale0)
  addPicture(get.fig.dir("NMR_Total", iso0 = iso0),
             sheet3, startRow = 230, startColumn = 1, scale = scale0)

  # Sheet 4 - Graph 5-24 ----
  sheet4 <- sheet[[4]]
  addPicture(get.fig.dir("5-14", iso0 = iso0),
             sheet4, startRow = 8, startColumn = 1, scale = scale0)
  addPicture(get.fig.dir("15-24", iso0 = iso0),
             sheet4, startRow = 45, startColumn = 1, scale = scale0)

  # Save
  if(!dir.exists(dir_save)) dir.create(dir_save, recursive = TRUE)
  file_out <- file.path(dir_save, paste0(cname0, "-CMR.xlsx"))
  saveWorkbook(wb, file = file_out)
  message(cname0, " CC profile saved in: ", file_out)
}

# Check -----

#' Read all "results.csv"
#'
#' Using \code{\link{read.results.csv.file}} to read all "results.csv" files
#'
#' @param results_dir_list results_dir_list list of all the results.csv files to
#'   read in and compare
#' @param year_range0 year range, default to all years 1931 to 2019
#'
#' @return dt_results_2020, saved as csv file in folder Results_Data too
#' @export read.all.results.csv
read.all.results.csv <- function(
  results_dir_list, year_range0 = 1931:2019
  ){
  # a list of all the results files:
  # combine original results
  # dt_test <- read.results.csv.file(results_dir_list$mr5t14.t.in.path, year_range = year_range0)
  dt_results_2020 <- rbindlist(lapply(results_dir_list, read.results.csv.file, year_range = year_range0))
  setnames(dt_results_2020, "value", "Results")
  ind_list <- list(
    "Under-five Mortality Rate" = "U5MR",
    "Infant Mortality Rate" = "IMR",
    "Child mortality rate age 1-4" = "CMR",
    "Neonatal Mortality Rate" = "NMR",
    "Mortality for 5-14 year-olds" = "10q5",
    "Mortality for 5-9 year-olds" = "5q5",
    "Mortality for 15-24 year-olds" = "10q15",
    "Mortality for 15-19 year-olds" = "5q15"
  )
  dt_results_2020[, Shortind:= get.match(Indicator, new_list = ind_list)]
  dt_results_2020[, table(Shortind)]
  dt_results_2020[, Quantile:= as.factor(Quantile)]
  # if(!identical(levels(dt_results_2020$Quantile), c("Lower", "Median", "Upper"))) stop("Check Quantile levels: ", paste(levels(dt_results_2020$Quantile), collapse = ", "))
  # dt_results_2020$Quantile <- factor(dt_results_2020$Quantile, levels = c("Median", "Lower", "Upper"))
  if(!dir.exists("Results_Data")) dir.create("Results_Data")
  fwrite(dt_results_2020, "Results_Data/IGME2020_afterCC_Results_all.csv")
  return(dt_results_2020)
}

#' Compare CC profile tables to "results.csv"
#'
#' Compare CC profile tables to all the "results.csv" files listed in
#' `results_dir_list` for all indicators enclosed.
#'
#' @importFrom readxl read_xlsx
#' @param cc_dir CC profile output directory: where the CC profiles are saved
#' @param results_dir_list list of all the results.csv files to read in and
#'   compare
#'
#' @export check.CC.profile.data
#' @return NULL, print problems as messages, save comparison datasets
check.CC.profile.data <- function(cc_dir, results_dir_list){
  if(!dir.exists(cc_dir))stop("Check if cc_dir is correct")
  if(any(!sapply(results_dir_list, file.exists))){
    stop("Check results.csv directory: ", paste(names(results_dir_list)[!sapply(results_dir_list, file.exists)], collapse = ", "))
  }

  cc_files_names <- list.files(cc_dir, full.names = FALSE)
  cc_files_names <- cc_files_names[!grepl("~", cc_files_names)]
  cc_files_names <- cc_files_names[!grepl("DO NOT SEND", cc_files_names)]
  cc_files <- list.files(cc_dir, full.names = TRUE)
  cc_files <- cc_files[!grepl("~", cc_files)]
  cc_files <- cc_files[!grepl("DO NOT SEND", cc_files)]
  message("There are in total ", length(cc_files), " CC profiles.")
  # all the vars to draw later
  vars <- do.call(paste, expand.grid(c("U5MR", "IMR", "NMR", "10q5", "10q15"), c("Median", "Lower", "Upper"), c("Total", "Male" ,"Female")))

  #' read CC xlsx profile data
  read.cc.profile <- function(i){
    message('\014')
    message("Reading CC profile:", i, ":", cc_files_names[i], ";", paste0(round(i / length(cc_files_names) * 100), "%\n"))
    cname <- sub("-CMR.xlsx", "", cc_files_names[i])
    cfile <- cc_files[i]
    suppressMessages(
      #Table Under 5
      c1 <- na.omit(setDT(readxl::read_xlsx(cfile, sheet = 1, skip = 7, n_max = 35)))
    )
    setnames(c1, "...1", "ind")
    # rename the indicators

    # make the variables' order correct
    c1$ind <- c(grep("U5MR", vars, value = TRUE), grep("IMR", vars, value = TRUE),
                grep("NMR", vars, value = TRUE)[1:3])
    c1_long <- melt(c1, id.vars = "ind", variable.name = "year", variable.factor = FALSE)

    # read "Table 5-24"
    suppressMessages(
      c2 <- na.omit(setDT(readxl::read_xlsx(cfile, sheet = 2, skip = 7, n_max = 15)))
    )
    setnames(c2, "...1", "ind")
    c2$ind <- c(grep("10q5", vars, value = TRUE)[1:3], grep("10q15", vars, value = TRUE)[1:3])
    c2_long <- melt(c2, id.vars = "ind", variable.name = "year", variable.factor = FALSE)
    total_long <- rbindlist(list(c1_long, c2_long))
    total_long$country <- cname
    return(total_long)
  }
  dt_cc <- rbindlist(lapply(1: length(cc_files_names), read.cc.profile))
  setcolorder(dt_cc, c("country",  "ind", "year", "value"))
  setnames(dt_cc, "value", "CC_Value")
  setkey(dt_cc, country)
  # obtain ISO.Code
  dt_cname <- setDT(readRDS("dt_new_cnames.Rds"))
  setkey(dt_cname, OfficialName)
  dt_cc <- dt_cname[,.(OfficialName, ISO3Code)][dt_cc]
  setnames(dt_cc, c("OfficialName", "ISO.Code", "Indicator",  "Year",  "CCProfile_Value"))
  dt_cc[, Year:= as.numeric(Year)]
  # fwrite(dt_cc, "output/Results_afterCC.csv")

  # 2. Load results.csv
  message("Read in all the results.csv\n")
  year_range0 <-sort(unique(dt_cc$Year))
  dt_results_2020 <- read.all.results.csv(results_dir_list = results_dir_list,
                                          year_range0 = year_range0)

  # 3. make comparison
  dt_results_2020[, ind:= paste(Shortind, Quantile, Sex)]
  dt_results_2020[, Year:= as.numeric(Year)]
  # subset indicators to those within the CC profile
  dt_results_2020 <- dt_results_2020[ind%in%unique(dt_cc$Indicator)]
  setkey(dt_results_2020, ISO.Code, ind, Year)
  setkey(dt_cc, ISO.Code, Indicator, Year)
  dt_compare <- dt_cc[,.(ISO.Code, Indicator, Year, CCProfile_Value)][dt_results_2020[,.(ISO.Code, ind, Year, Results)]]
  dt_compare[, Results_1 := roundoff(Results, 1)]
  dt_compare[, diff :=  Results_1 - CCProfile_Value]
  if(length(dt_compare[is.na(diff), unique(Indicator)])>0) message("NA caused by missing data in the CC profile in these indicators: ", paste(dt_compare[is.na(diff), unique(Indicator)], collapse = ", "))
  if(length(dt_compare[is.na(diff), unique(ISO.Code)])>0) message("NA caused by missing data in the CC profile in these indicators: ", paste(dt_compare[is.na(diff), unique(ISO.Code)], collapse = ", "))
  if(mean(dt_compare$diff, na.rm = TRUE)==0) {
    message("Check passed, cqt files match with results.csv\n")
  } else {
    message("Check the following ", dt_compare[diff!=0, uniqueN(Indicator)]," indicators with unmatched values: ", paste(dt_compare[diff!=0, unique(Indicator)], collapse = ", "))
    message("Check the following ",dt_compare[diff!=0, uniqueN(ISO.Code)] ," countries with unmatched values: ", paste(dt_compare[diff!=0, unique(ISO.Code)], collapse = ", "))
    message("comparison file saved as: ", "Compare_CCProfile_vs_Results.csv")
    fwrite(dt_compare, "Compare_CCProfile_vs_Results.csv")
  }
}

#' Compare the saved cqt vs results
#'
#' Should be the same as compare CC profiles vs results. It is used to check if
#' the cqt file and CC profile are both updated in the same time
#'
#' @param dt_results obtained by `read.all.results.csv(results_dir_list)`
#' @param dt_cqt obtained by `get.dt.cqt(dir_cqt_files)`
#' @return NULL
#' @export compare.results.vs.cqt
compare.results.vs.cqt <- function(dt_results, dt_cqt){
  setkey(dt_results, ISO.Code, Quantile, Shortind, Year, Sex)
  setkey(dt_cqt, ISO.Code, Quantile, Shortind, Year, Sex)
  dt_results_vs_cqt <- dt_results[dt_cqt]
  dt_results_vs_cqt[, diff :=  roundoff(Results,1) - roundoff(Value,1)]
  dt_results_vs_cqt[diff!=0, ]
  message("\n")
  if(mean(dt_results_vs_cqt$diff, na.rm = TRUE)==0) {
    message("Check passed, cqt files match with results.csv\n")
  } else {
    message("Check the following ", dt_results_vs_cqt[diff!=0, uniqueN(Indicator)]," indicators with unmatched values: ", paste(dt_results_vs_cqt[diff!=0, unique(Indicator)], collapse = ", "))
    message("Check the following ",dt_results_vs_cqt[diff!=0, uniqueN(ISO.Code)] ," countries with unmatched values: ", paste(dt_results_vs_cqt[diff!=0, unique(ISO.Code)], collapse = ", "))
  }
}

# Make cqt file from other sources -------------------------------------------

#' Get cqt file from final aggregated results for plotting, based on the
#' long-formatted final results
#'
#' @param dt_long final results in long format
#' @param output_dir where to find the "mcmc.meta.rda"
#' @param mcmc.meta_filename default to "mcmc.meta.rda"
#'
#' @return res.cqt.Lw.new
#' @export get.cqt.from.aggfinal
#' @examples
#' \dontrun{
#' dt_u5 <- get.CME.UI.data(dir_file = final_dir_list$dir_total_2020)
#' dt_u5 <- dt_u5[Indicator == "U5MR"]
#' output_dir <- file.path(get.IGMEoutput.dir(2019), "GR20200214_all")
#' res.cqt.Lw.new <- get.cqt.from.aggfinal(dt_u5, output_dir)
#' }
#'
get.cqt.from.aggfinal <- function(
  dt_long,
  output_dir,
  mcmc.meta_filename = "mcmc.meta.rda"
){
  load(file.path(output_dir, mcmc.meta_filename))
  iso_order <- mcmc.meta$data.all$iso.c
  load(file.path(output_dir, "year.t.rda"))
  years0 <- year.t
  setnames(dt_long, upper.first.letter(colnames(dt_long)))
  dt_long[, years:= Year + 0.5]
  setorder(dt_long, ISO3Code, Quantile, years)
  dt_long <- dt_long[,.(ISO3Code, Quantile, years, Value)]
  years0_ava <- sort(unique(dt_long$years))
  year_extra <- year.t[!year.t%in%years0_ava]
  #
  dt_longw <- dcast(dt_long,ISO3Code+Quantile~years, value.var = "Value")
  # Inject the extra years
  # It is critical to have the same year as `year.t` in dimension
  dt_longw2 <- as.data.table(matrix(rep(NA_real_, nrow(dt_longw) * length(year_extra)), nrow = nrow(dt_longw)))
  setnames(dt_longw2, as.character(year_extra))
  dt_longw3 <- cbind(dt_longw, dt_longw2)
  # melt back into long
  dt_long2 <- melt(dt_longw3, id.vars = c("ISO3Code", "Quantile"), variable.name = "years", variable.factor = FALSE)
  setorder(dt_long2, ISO3Code, Quantile, years)
  all(years0_ava%in%years0)
  # match ISO order
  dt_long2 <- dt_long2[order(match(ISO3Code, rep(iso_order, each = length(years0))))]
  setorder(dt_long2, years, Quantile) # set the right order is the key to produce right array
  # Now the order is by t (year), q, and c (iso)
  cqt <- array(data = dt_long2[, value],
               dim = c(length(iso_order),
                       3,
                       length(years0)),
               dimnames = list(c = iso_order,
                               q = c(0.05, 0.5, 0.95),
                               t = years0))
  cqt[1,,]
  res.cqt.Lw.new <- list()
  res.cqt.Lw.new[["0.5"]] <- cqt
  return(res.cqt.Lw.new)
  # save(res.cqt.Lw.new, file =  file.path(output_dir, "res.cqt.Lw.new.rda"))
}


#' Make cqt file from `results.csv` file
#'
#' Read `results.csv` and `mcmc.meta` files in the output dir and make res.cqt.Lw.rda
#'
#' @param output_dir where to find the results.csv
#' @param mcmc.meta_filename default to "mcmc.meta.rda"
#' @param results_filename default to "results.csv", the file to read
#' @param res.cqt_filename default to "res.cqt.Lw.rda", file name for saving
get.cqt.from.results <- function(
  output_dir,
  mcmc.meta_filename = "mcmc.meta.rda",
  results_filename = "results.csv",
  res.cqt_filename = "res.cqt.Lw.rda"
){
  load(file.path(output_dir, mcmc.meta_filename))
  iso_order <- mcmc.meta$data.all$iso.c
  load(file.path(output_dir, "year.t.rda"))
  years <- year.t
  dt <- fread(file.path(output_dir, results_filename))
  vars_wanted <- c("ISO.Code", "Quantile", paste0("X", years))
  dt_long <- melt(dt[,..vars_wanted], measure.vars = paste0("X", years), variable.factor = FALSE)
  dt_long[, years:=as.numeric(sub("X", "", variable))]
  dt_long <- dt_long[order(match(ISO.Code, rep(iso_order, each = length(years))))]
  setorder(dt_long, years, Quantile) # set the right order is the key to produce right array
  # Now the order is by t (year), q, and c (iso)
  cqt <- array(data = dt_long[, value],
               dim = c(length(iso_order),
                       3,
                       length(years)),
               dimnames = list(c = iso_order,
                               q = c(0.05, 0.5, 0.95),
                               t = years))
  res.cqt.Lw <- list()
  res.cqt.Lw[["0.5"]] <- cqt
  save(res.cqt.Lw, file =  file.path(output_dir, res.cqt_filename))
}


