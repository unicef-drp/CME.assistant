# helper functions to make plots
# Yang Liu
# 19/10/18


#' a label function to relabel certain variable names
#'
#' You can provide a __new_list__ to define the labels. If any label is not
#' provided, the function will just return the original value
#'
#' @param x a element or a vector
#' @param new_list if you supply a new list the function will use instead of the
#'   default_labels
#' @param no_line_break to remove \n from the string
#' @export get.label
#' @return updated labels as character vector
get.label <- function(x, new_list = NULL, no_line_break = FALSE){
  default_labels <- list(
    "2"  = "HIV/AIDS",
    "3"  = "Diarrhoea",
    "5"  = "Tetanus",
    "6"  = "Measles",
    "7"  = "Meningitis\n/encephalitis",
    "8"  = "Malaria",
    "9"  = "Pneumonia",
    "10" = "Prematurity",
    "11" = "Birth asphyxia and birth trauma",
    "12" = "Sepsis",
    "13" = "Other Group 1",
    "15" = "Congenital anomalies",
    "16" = "Other noncommunicable diseases",
    "17" = "Injuries",
    "57" = "Tetanus\n/meningitis\n/encep",  # 5+7
    # WHO region names
    "1_Afr" = "Africa",
    "2_Amr" = "Americas",
    "3_Sear"= "South-East Asia",
    "4_Eur" = "Europe",
    "5_Emr" = "Eastern Mediterranean",
    "6_Wpr" = "Western Pacific"
  )
  if(is.null(new_list)){
    labs <- default_labels
  } else {
    if(is.list(new_list)){
      labs <- new_list
    } else {
      message("new_list must be a list. Still use the default list.")
      labs <- default_labels
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

#' use ggsave to save figs in both png and pdf
#'
#' use ggsave to save figs in both png and pdf, default to 600dpi, store in working
#' folder unless `folder_name` is named.
#' @importFrom ggplot2 ggsave
#' @importFrom here here
#' @param file_name the name of the plot file, e.g, "plot1"
#' @param myplot ggplot2 object
#' @param width width for ggsave
#' @param height height for ggsave
#' @param folder_name the name of the folder, e.g. "figure", if provided, the
#'   directory will be created if not there
#' @export ggsave.figs
#' @return NULL
ggsave.figs <- function(myplot, file_name = "myplot", width = 8, height = 6, folder_name = ""){
  fig_names <- paste0(file_name, c(".png", ".pdf"))
  if(!dir.exists(here::here(folder_name))) dir.create(here::here(folder_name))
  ggsave(here::here(folder_name, fig_names[1]), plot = myplot,
         dpi = 600, device = "png",  width = width, height = height)
  ggsave(here::here(folder_name, fig_names[2]), plot = myplot,
         dpi = 600, device = "pdf",  width = width, height = height)
}


#' format by comma
#' @param x param to be rounded, e.g, 1000 -> "1,000"
#' @importFrom scales comma
#' @export fc
#' @return charactor
fc <- function(x){
  scales::comma(round(x))
}
