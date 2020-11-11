## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(CME.assistant)

## ---- eval = TRUE-------------------------------------------------------------
# without supplying dir_file, by default returns the published IGME 2020 final
# results
dt_1 <- get.CME.UI.data(use_IGME_year = 2020)
head(dt_1)

# can read final output file from Dropbox for any indicator
dir_list <- load.final_dir()
dt_2 <- get.CME.UI.data(dir_file = dir_list$dir_female_2020)
head(dt_2)

## ---- eval = FALSE------------------------------------------------------------
#  input_dirs <- load.IGMEinput.dir()
#  dir_U5MR   <- get.dir_U5MR(input_dirs$dir_IGME_20)
#  dir_IMR    <- get.dir_IMR(input_dirs$dir_IGME_20)
#  dir_NMR    <- get.dir_NMR(y5 = TRUE) # either 5-year or not
#  dir_gender <- get.dir_gender(plotting = TRUE) # either dataset for plotting or modeling

## -----------------------------------------------------------------------------
# a helpful function to revise variable, you can provide a __new_list__ to define the labels
get.match(c("a","b", "c"), new_list = list("a" = "new label 1",
                                           "b" = "new label 2"))


## -----------------------------------------------------------------------------
str(UNICEF_colors)

## ---- eval = FALSE------------------------------------------------------------
#  # Write several dataset into one xlsx file:
#  save.xlsx.XLConnect(file_dir = "temp.xlsx", list_of_dt = list(mtcars[1:5,]))

