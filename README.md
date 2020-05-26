# CME.assistant

<!-- badges: start -->
<!-- badges: end -->
__Under development__

The goal of CME.assistant is to store and maintain most frequently used reusable functions

All the information included were published thus this package wss made public

## Installation
To install from github: 
* All the examples are in the vignettes, set `build_vignettes = TRUE`
* May need to add `build_opts` to avoid potential Java problem according to [this post](https://github.com/salimk/Rcrawler/issues/1)

```{r}
devtools::install_github("unicef-drp/CME.assistant")
# or if needed,
# devtools::install_github("unicef-drp/CME.assistant", build_vignettes = TRUE, build_opts = "--no-multiarch")
library(CME.assistant)
```
It is also a good idea to download the repo, further revise and use 'Ctrl+Shift+B' to compile locally.
```{r}
git clone https://github.com/unicef-drp/CME.assistant.git
```
## To view vignette
```{r}
vignette(package = "CME.assistant")
vignette("CMEvignette1")
```

