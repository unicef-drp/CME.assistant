# Data engineering related helper functions
# CME functions package
# more to be added
# Yang Liu


# Read data ---------------------------------------------------------------


#' get three types (Under-five Infant Neonatal) from Rates & Deaths summary
#' @import data.table
#' @param year_range a vector of years we want, default to 2000:2018
#' @param get_what "Deaths" or "Rate", default to "Rate": get the three CME rate
#' @examples
#' get.CME.data(year_range = c(2016:2018))
#' @export get.CME.data
#' @return dt of ISO3, UNcode, year, Under-five, Infant, Neonatal, one row for each country each year in year_range
#'
get.CME.data <- function(year_range = c(2000:2018), get_what = NULL){

  data(Rates_Deaths_Country_Summary_2019)
  dt <- Rates_Deaths_Country_Summary_2019
  available_years <- readr::parse_number(grep("IMR", names(dt), value = TRUE))
  if (!all(year_range%in%available_years)) {
    warning("Available years are between: ", paste(range(available_years), collapse = " and "),
            ". Set years to default range.")
    year_range <- c(2000:2018) # set to default range
  }

  if(is.null(get_what)) {
    CME_types_full <- c("U5MR", "IMR", "NMR")
    vars_wanted <- c("ISO3Code",	"UNCode",
                     do.call(paste, expand.grid(CME_types_full, year_range)))
  } else {
    CME_types_full <- c("Under-five", "Infant", "Neonatal")
    # get all the combination for variable names: e.g. Under-five Deaths 2000, 19*3 = 57 variables
    vars_wanted <- c("ISO3Code",	"UNCode",
                     do.call(paste, expand.grid(CME_types_full, "Deaths", year_range)))
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
  data("country_info")
  dt <- country_info
  # UNICEFReportRegion2 offers subregions for ECA and SSA, combined into UNICEFReportRegion
  dt[, UNICEFReportRegion:=UNICEFReportRegion1]
  dt[UNICEFReportRegion2!="", UNICEFReportRegion:=UNICEFReportRegion2]
  return(dt)
}


# Output data -------------------------------------------------------------


#' function to write several dataset into excel file (one dataset each sheet) with table format
#'
#' revised based on http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r
#' @param file the path to the output file
#' @import xlsx
#' @export xlsx.writeMultipleData
#' @param ...  a list of data to write to the workbook
#' @param sheet_name must be a vector of sheet names you want, if length != length of object, it will be ignored
#' @examples xlsx.writeMultipleData("myworkbook.xlsx", mtcars, Titanic, sheet_name = c("dt1", "dt2"))
#'
xlsx.writeMultipleData <- function (file, ..., sheet_name = NULL)
{
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  # if length (sheet_name) != length of object, sheet_name will be ignored
  objnames <- if(is.null(sheet_name)|(length(sheet_name)!=length(objects)))as.character(fargs)[-c(1, 2)] else sheet_name
  nobjects <- length(objects)

  wb <- createWorkbook(type="xlsx")
  # cell style could be further revised in the future
  CellStyle(wb, dataFormat=NULL, alignment=NULL,
            border=NULL, fill=NULL, font=NULL)
  # set some format
  TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
  TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"),
           pen=c("BORDER_THIN", "BORDER_THICK"))

  sheets_list <- lapply(objnames, createSheet, wb = wb)
  for (i in 1:nobjects) {
      addDataFrame(objects[[i]], sheet = sheets_list[[i]],
                   colnamesStyle = TABLE_COLNAMES_STYLE,
                   rownamesStyle = TABLE_ROWNAMES_STYLE)
  }
  saveWorkbook(wb, file)
}


if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("."))
}
