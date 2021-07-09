# CME.assistant


[![CRAN version](http://www.r-pkg.org/badges/version/CME.assistant)](https://cran.r-project.org/package=CME.assistant) 

<!-- badges: start -->
<!-- badges: end -->

The goal of `CME.assistant` is to collect frequently used functions for data enginerring for internal use.

## Installation  

To install from github: 

```{r}
devtools::install_github("unicef-drp/CME.assistant")
# or if need vignette,
devtools::install_github("unicef-drp/CME.assistant", build_vignettes = TRUE)
library(CME.assistant)
```
Skip the recommended update of other packages if you don't like. 
Alternative option: download the repo, and use 'Ctrl/Cmd+Shift+B' to compile the package locally.
Maybe also need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) if not installed yet. 

## To view vignette
```{r}
devtools::install_github("unicef-drp/CME.assistant", build_vignettes = TRUE)
vignette(package = "CME.assistant")
vignette("CMEvignette1")
```

