# CME.assistant

<!-- badges: start -->
<!-- badges: end -->
__Under development__

The goal of CME.assistant is to store and maintain most frequently used reusable functions

All the data included were published thus this package is non-private

## Installation
To install from github: 
* Examples in the vignettes, set `build_vignettes = TRUE`
* May need to add `build_opts` to avoid potential Java problem according to [this post](https://github.com/salimk/Rcrawler/issues/1)
* Update R and RStudio to the latest version if there are still weird problems

```{r}
devtools::install_github("unicef-drp/CME.assistant")
# or if needed,
devtools::install_github("unicef-drp/CME.assistant", build_vignettes = TRUE, build_opts = "--no-multiarch")
library(CME.assistant)
```
Another alternative: download the repo, and use 'Ctrl+Shift+B' to compile the package locally.
```{r}
git clone https://github.com/unicef-drp/CME.assistant.git
```

## To view vignette
```{r}
devtools::install_github("unicef-drp/CME.assistant", build_vignettes = TRUE)
vignette(package = "CME.assistant")
vignette("CMEvignette1")
```

