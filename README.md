# CME.assistant


<!-- badges: start -->
<!-- badges: end -->

The goal of `CME.assistant` is to collect frequently used functions, mostly for data engineering, for internal use.
The only package dependency is `data.table`.

## Installation  

To install from Github: 

```{r}
devtools::install_github("unicef-drp/CME.assistant")
# or if need vignette,
devtools::install_github("unicef-drp/CME.assistant", build_vignettes = TRUE)
library(CME.assistant)
```
Skip the recommended update of other packages if you don't like.   

Alternative option: download the repo and compile the package locally. (Also need [Rtools](https://cran.r-project.org/bin/windows/Rtools/) if not installed yet.)

## To view vignette
```{r}
devtools::install_github("unicef-drp/CME.assistant", build_vignettes = TRUE)
vignette(package = "CME.assistant")
vignette("CMEvignette1")
```

