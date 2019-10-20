# CME.assistant

<!-- badges: start -->
<!-- badges: end -->

The goal of CME.assistant is to share Reuseable Helper Functions for CME Team

## Installation

Please install from github:
``` {r}
devtools::install_github("liuyanguu/SHAPforxgboost")
```

## UNICEF color scheme
```{r}
library(CME.assistant)
str(UNICEF_colors)
```

## Functions for data engineering
### Input
```{r}
# get CME mortality rate for certain years 
get.CME.data(year_range = c(2016:2018))[1:3]

# get CME death numbers for certain years
get.CME.data(year_range = c(2016:2018), "deaths")[1:3]

# get country info
get.country.info()[1:3, 1:5]
```

### Output
```{r}
# Write several dataset into one xlsx file:
xlsx.writeMultipleData("myworkbook.xlsx", mtcars, Titanic, sheet_name = c("dt1", "dt2"))
# file.remove("myworkbook.xlsx")
```

## Functions for making plots
```{r}
# use `ggsave.figs` to save ggplot into both pdf and png
# P.S. __SHAPforxgboost__ was my last package offering some simple ggplot visualization functions
ggplot_sample <- SHAPforxgboost::scatter.plot.diagonal(data = iris, x = "Sepal.Length", y = "Petal.Length")
ggsave.figs(ggplot_sample, file_name = "Iris_sample", folder_name = "figures")

# revise labels
get.label(c("2", "3"))
```

