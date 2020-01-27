# CME.assistant

<!-- badges: start -->
<!-- badges: end -->
__Under development__

The goal of CME.assistant is to store some reusable helper functions
It doesn't contain any unpublished information. 

## Installation
To install from github: 
* won't work if the repo is private. 
* need to INSTALL_opts to avoid Java problem according to [this post](https://github.com/salimk/Rcrawler/issues/1)
```{r}
devtools::install_github("unicef-drp/CME.assistant", build_vignettes = TRUE, INSTALL_opts = "--no-multiarch")
library(CME.assistant)
```
It is also a good idea to download, further revise and use 'Ctrl+Shift+B' to compile locally.
```{r}
git clone https://github.com/unicef-drp/CME.assistant.git
```
## To view vignette
```{r}
vignette(package = "CME.assistant")
vignette("CMEvignette1")
```

