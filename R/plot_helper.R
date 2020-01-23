# helper functions to make plots
# Yang Liu
# 19/10/18


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
