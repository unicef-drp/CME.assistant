# CME.assistant


[![CRAN version](http://www.r-pkg.org/badges/version/CME.assistant)](https://cran.r-project.org/package=CME.assistant) 

<!-- badges: start -->
<!-- badges: end -->

The goal of CME.assistant is to collect some frequently used functions.
This package is made non-private since it only attaches published final estimates for the recent 2 years.

## Installation  

To install from github: 
* Examples in the vignettes, set `build_vignettes = TRUE`
* Maybe need to add `build_opts = "--no-multiarch"` if encounter Java-related problem according to [this post](https://github.com/salimk/Rcrawler/issues/1)

```{r}
devtools::install_github("unicef-drp/CME.assistant")
# or if needed,
devtools::install_github("unicef-drp/CME.assistant", build_vignettes = TRUE, build_opts = "--no-multiarch")
library(CME.assistant)
```
Alternative option: download the repo, and use 'Ctrl/Cmd+Shift+B' to compile the package locally.

## To view vignette
```{r}
devtools::install_github("unicef-drp/CME.assistant", build_vignettes = TRUE)
vignette(package = "CME.assistant")
vignette("CMEvignette1")
```

