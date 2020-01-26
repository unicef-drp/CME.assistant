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
#' @examples save.xlsx.XLConnect(file_dir = "temp.xlsx", list_of_dt = list(mtcars[1:5,]))
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
  # check dimensions of dataset
  if(!all(lapply(list_of_dt, function(x) length(dim(x))) == 2)) stop (
    "Dataset in list_of_dt should have dimension of 2. ",
    "dim(dt) are ", paste(lapply(list_of_dt, dim), collapse = ", "))

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
  message("Xlsx file saved as: ", file_dir)
}


