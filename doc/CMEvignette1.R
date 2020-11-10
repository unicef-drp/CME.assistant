## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(CME.assistant)
str(UNICEF_colors)

## ---- eval = FALSE------------------------------------------------------------
#  # without supplying dir_file, by default returns the published IGME 2020 final
#  # results
#  dt_1 <- get.CME.UI.data(use_IGME_year = 2020)
#  rmarkdown::paged_table(dt_1)
#  
#  # can read final output file from Dropbox for any indicator
#  dir_list <- load.dir_list()
#  dt_2 <- get.CME.UI.data(dir_file = dir_list$dir_female_2020)
#  rmarkdown::paged_table(dt_2)

## -----------------------------------------------------------------------------
# a helpful function to revise variable, you can provide a __new_list__ to define the labels
get.match(c("a","b", "c"), new_list = list("a" = "new label 1",
                                           "b" = "new label 2"))

## -----------------------------------------------------------------------------
# Write several dataset into one xlsx file:
# save.xlsx.XLConnect(file_dir = "temp.xlsx", list_of_dt = list(mtcars[1:5,]))
# unlink("temp.xlsx")

