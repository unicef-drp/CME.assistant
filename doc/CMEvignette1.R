## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(CME.assistant)
str(UNICEF_colors)

## ------------------------------------------------------------------------
dt_cme_long <- get.CME.estimates.long(ind = "U5MR", c_name = "Mozambique",
                                      year_range = 2000:2018)
dt_cme_wide <- get.dt.wide(dt_cme_long)

rmarkdown::paged_table(dt_cme_long[1:5])
rmarkdown::paged_table(dt_cme_wide[1:5])

## ------------------------------------------------------------------------
# Write several dataset into one xlsx file:
save.xlsx.XLConnect(file_dir = "temp.xlsx", list_of_dt = list(mtcars[1:5,]))

## ---- fig.width = 8, fig.height = 6--------------------------------------
# use `ggsave.figs` to save ggplot into both pdf and png
# P.S. __SHAPforxgboost__ was my last package offering some simple ggplot visualization functions
ggplot_sample <- ggplot2::qplot(data = iris, x = Sepal.Length, y = Petal.Length)
ggsave.figs(ggplot_sample, file_name = "Iris_sample", folder_name = "figures")
# to remove the file just saved
unlink(here::here("figures"), recursive = TRUE)

# Ads: if you wish to give a try, __SHAPforxgboost__ was my last package offering some ggplot visualization functions
# install.packages("SHAPforxgboost")
SHAPforxgboost::scatter.plot.diagonal(data = iris, x = "Sepal.Length", y = "Petal.Length")

## ------------------------------------------------------------------------
# a helpful function to revise labels, you can provide a __new_list__ to define the labels
get.match(c("2", "3"))
get.match(c("a","b", "c"), new_list = list("a" = "new label 1",
                                           "b" = "new label 2"))


