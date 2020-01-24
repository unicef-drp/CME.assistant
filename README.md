# CME.assistant

<!-- badges: start -->
<!-- badges: end -->
__Under development__

The goal of CME.assistant is to store some reusable helper functions

## Installation

To install from github: / not working if the repo is private. 
```{r}
devtools::install_github("unicef-drp/CME.assistant")
library(CME.assistant)
```
It is also a good idea to download, further revise and use 'Ctrl+Shift+B' to compile locally.
```{r}
git clone https://github.com/unicef-drp/CME.assistant.git
```

## UNICEF color scheme: `UNICEF_colors`
```{r}
library(CME.assistant)
str(UNICEF_colors)
```

## Functions for data engineering
### Data input
**`get.CME.data`**
Get publised 2019 CME mortality rate for certain years 
Source: [childmortality.org](https://childmortality.org/)

```{r}
get.CME.data(year_range = c(2016:2018))[1:3]

get.CME.data(year_range = c(2016:2018), "deaths")[1:3]
```

### Data output
**`xlsx.writeMultipleData`**
```{r}
# Write several dataset into one xlsx file:
xlsx.writeMultipleData("myworkbook.xlsx", mtcars, Titanic, sheet_name = c("dt1", "dt2"))
# to remove the file just saved
file.remove("myworkbook.xlsx")
```

## Functions for making plots
**`ggsave.figs`**
```{r}
# use `ggsave.figs` to save ggplot into both pdf and png
# P.S. __SHAPforxgboost__ was my last package offering some simple ggplot visualization functions

ggplot_sample <- ggplot2::qplot(data = iris, x = Sepal.Length, y = Petal.Length)
ggsave.figs(ggplot_sample, file_name = "Iris_sample", folder_name = "figures")
# to remove the file just saved
unlink(here::here("figures"), recursive = TRUE)


# Ads: if you wish to give a try, __SHAPforxgboost__ was my last package offering some simple ggplot visualization functions
install.packages("SHAPforxgboost")
SHAPforxgboost::scatter.plot.diagonal(data = iris, x = "Sepal.Length", y = "Petal.Length")
```
**`get.label`**
```{r}
# a helpful function to revise labels, you can provide a __new_list__ to define the labels
get.label(c("2", "3"))
get.label(c("a","b", "c"), new_list = list("a" = "a new label"))

```

