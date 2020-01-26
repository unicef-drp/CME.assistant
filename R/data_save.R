# Output data -------------------------------------------------------------

#' save a list of dts as one xlsx
#' @import XLConnect
#' @export save.xlsx.XLConnect
#'
#' @param sheet_names a vector of sheet names
#' @param list_of_dt list of dt
#' @param file_dir if NULL, save as temp.xlsx
#' @param data_sources  vector of sources
#' @param ColumnWidth0  default to 4000
#'
#' @examples save.xlsx.XLConnect(file_dir = "temp.xlsx", list_of_dt = list(mtcars = mtcars[1:5,]))
save.xlsx.XLConnect <- function(file_dir = NULL,
                                list_of_dt,
                                sheet_names = NULL,
                                data_sources = NULL,
                                title_style = "border",
                                ColumnWidth0 = 4000){
  require("XLConnect")
  options(java.parameters = "-Xmx1024m" )
  if(is.null(file_dir)) file_dir <- here::here("temp.xlsx")
  if(file.exists(file_dir))file.remove(file_dir)
  #Loading an Excel workbook. Both .xls and .xlsx file formats can be used.
  wb0 = loadWorkbook(file_dir, create = TRUE)
  if(is.null(sheet_names)) sheet_names <- paste0("Data", 1:length(list_of_dt))
  if(is.null(data_sources)) data_sources <- rep("", length(list_of_dt))
  write.sheet <- function(sheet_name, dt, data_source, wb = wb0){
    message("dim dt is ", dim(dt))
    createSheet(wb, name = sheet_name)
    createName(wb, name = "data_region", formula = paste0(sheet_name, "!$A$1"), overwrite = TRUE)
    writeNamedRegion(wb, dt, name = "data_region")
    # format for the title row
    cs_title <- createCellStyle(wb)
    setWrapText(cs_title, wrap = TRUE) # wrap first row

    # set title style
    if(title_style == "border"){ # bottom border
      setBorder(cs_title, side = c("bottom"), type = XLC$BORDER.MEDIUM, color = c(XLC$COLOR.BLACK))
    } else { # fill with grey
      setFillPattern(cs_title, fill = XLC$"FILL.SOLID_FOREGROUND")
      setFillForegroundColor(cs_title, color = XLC$"COLOR.GREY_25_PERCENT")
    }
    # 1/25 I was unable to set font bold in XLConnect
    setCellStyle(wb, sheet = sheet_name, row = 1, col = 1:ncol(dt), cellstyle = cs_title)

    # set filter
    setAutoFilter(wb, sheet = sheet_name, reference = aref("A1", dim(dt)))
    setColumnWidth(wb, sheet = sheet_name, column = 1:ncol(dt), width = ColumnWidth0)
    # write sources
    writeWorksheet(wb, data = data_source, sheet = sheet_name, startRow = nrow(dt) + 3, startCol = 1,
                   header = FALSE)

  }
  invisible(Map(write.sheet, sheet_names, list_of_dt, data_sources))
  saveWorkbook(wb0)
}



#' function to write several dataset into excel file (one dataset each sheet) with table format
#' 2009/10
#' revised based on http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r
#' @import xlsx
#' @export save.xlsx
#' @param file the path to the output file
#' @param ...  a list of data to write to the workbook
#' @param sheet_name must be a vector of sheet names you want, if length != length of object, it will be ignored
#' @examples save.xlsx("myworkbook.xlsx", list(mtcars, Titanic), sheet_name = c("dt1", "dt2"))
#'
save.xlsx <- function (file_dir = NULL,
                       list_of_dt,
                       end_notes = NULL,
                       sheet_name = NULL)
{
  if(is.null(file_dir)) file_dir <- here::here("temp.xlsx")
  # each dt is an object
  n_obj <- length(list_of_dt)
  objnames <- if(is.null(sheet_name)|(length(sheet_name)!=length(list_of_dt))) paste0("Data_", 1:n_obj)  else sheet_name

  # funcs
  add.string <- function(sheet, rowIndex, colIndex,
                        string0,
                        stringStyle = CellStyle(wb)){
    rows <-createRow(sheet, rowIndex=rowIndex)
    sheetTitle <-createCell(rows, colIndex)
    setCellValue(sheetTitle[[1,1]], string0)
    setCellStyle(sheetTitle[[1,1]], stringStyle)
  }

  # create a blank work book
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
  for (i in 1:n_obj) {
    addDataFrame(list_of_dt[[i]], sheet = sheets_list[[i]],

                 colnamesStyle = TABLE_COLNAMES_STYLE,
                 rownamesStyle = TABLE_ROWNAMES_STYLE)
    if(!is.null(end_notes)){
      note_style <- CellStyle(wb) + Font(wb,  heightInPoints = 11, isBold = FALSE)
      add.string(sheets_list[[i]], rowIndex=nrow(list_of_dt[[i]])+2, colIndex =1,
                string = end_notes[i], stringStyle = note_style)

    }
  }
  saveWorkbook(wb, file_dir)
}

# something to please the cran, not that necessary
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c(".",
                           "Rates_Deaths_Country_Summary_2019",
                           "..vars_wanted", "patterns", "ISO3Code", "country_info",
                           "UNICEFReportRegion", "UNICEFReportRegion1", "UNICEFReportRegion2",
                           "default_label_1"))
}
