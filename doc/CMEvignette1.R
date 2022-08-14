## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message=FALSE, warning=FALSE, 
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
library("CME.assistant")

## ---- eval = FALSE------------------------------------------------------------
#  # reproduce these code requires access to our internal Dropbox folders
#  
#  # Always a good idea to update the library to make sure you have the latest version
#  # devtools::install_github("unicef-drp/CME.assistant")
#  library("CME.assistant")
#  USERPROFILE <- CME.assistant::load_os_leading_dir() # leading dir to Dropbox
#  
#  # Dropbox directories to all results.csv
#  dir_CC_code <- file.path(USERPROFILE, "Dropbox/UNICEF Work/Country consultation/Code_for_CC")
#  source(file.path(dir_CC_code, "R/Dropbox_results_directories_2021.R"))
#  dt_results <- rbindlist(lapply(results_dir_list_final_2021, CME.assistant::read.results.csv))
#  # which the same as:
#  dt_results <- do.call(rbind, lapply(results_dir_list_final_2021, CME.assistant::read.results.csv))
#  dt_results[!is.na(value), table(Sex, Shortind)]
#  
#  
#  # Dropbox directories to all final aggregates
#  dir_report_code <- file.path(USERPROFILE, "Dropbox/UNICEF Work/IGME report etc/2021/Code_for_report/")
#  source(file.path(dir_report_code, "Dropbox_aggresults_directories_2021.R"))
#  dir_country_summary <- c(
#    file.path(dir_aggu5,      "Rates & Deaths_Country Summary.csv"),
#    file.path(dir_aggu5_f,    "Rates & Deaths(ADJUSTED)_female_Country Summary.csv"),
#    file.path(dir_aggu5_m,    "Rates & Deaths(ADJUSTED)_male_Country Summary.csv"),
#    file.path(dir_agg10q5,    "Rates & Deaths_Country Summary.csv"),
#    file.path(dir_agg10q5_f,  "Rates & Deaths(ADJUSTED)_Country Summary.csv"),
#    file.path(dir_agg10q5_m,  "Rates & Deaths(ADJUSTED)_Country Summary.csv"),
#    file.path(dir_agg10q15,   "Rates & Deaths_Country Summary.csv"),
#    file.path(dir_agg10q15_f, "Rates & Deaths(ADJUSTED)_Country Summary.csv"),
#    file.path(dir_agg10q15_m, "Rates & Deaths(ADJUSTED)_Country Summary.csv")
#  )
#  dt_estimates <- rbindlist(lapply(dir_country_summary, CME.assistant::read.country.summary))
#  dt_estimates[, table(Shortind, Sex)]

## ---- eval = FALSE------------------------------------------------------------
#  region_group_filename <- "SDGSimpleRegion"
#  dir_region_summary <- c(
#    file.path(dir_aggu5,   paste0("Rates & Deaths_", region_group_filename, ".csv")),
#    file.path(dir_aggu5_f, paste0("Rates & Deaths(ADJUSTED)_female_", region_group_filename, ".csv")),
#    file.path(dir_aggu5_m, paste0("Rates & Deaths(ADJUSTED)_male_", region_group_filename, ".csv")),
#    file.path(dir_agg10q5,   paste0("Rates & Deaths_", region_group_filename, ".csv")),
#    file.path(dir_agg10q15,   paste0("Rates & Deaths_", region_group_filename, ".csv"))
#  )
#  dt_region <- rbindlist(lapply(dir_region_summary, CME.assistant::read.region.summary))

## ---- eval = FALSE------------------------------------------------------------
#  dir_cs_u5 <- file.path(dir_aggu5, "Rates & Deaths_Country Summary.csv")
#  dt_1 <- get.CME.UI.data(dir_file = dir_cs_u5)
#  dt_1[Year == 2020][1:3,]
#  dt_1 <- get.CME.UI.data(dir_file = dir_cs_u5,
#                          idvars = c("ISO3Code", "CountryName", "OfficialName"), format = "wide_q")
#  dt_1[Year == 2020][1,]
#  dt_1 <- get.CME.UI.data(dir_file = dir_cs_u5, format = "wide_q")
#  dt_1[Year == 2020][1,]
#  dt_1 <- get.CME.UI.data(dir_file = dir_cs_u5, format = "wide_ind", round_digit = 1)
#  dt_1[Year == 2020][1:3,]
#  dt_1 <- get.CME.UI.data(dir_file = dir_cs_u5, format = "wide_get", round_digit = 1)
#  dt_1[Year == 2020][1:3,]
#  dt_wy <- get.CME.UI.data(dir_file = dir_cs_u5, format = "wide_year", year_range = c(2000, 2010, 2020))
#  dt_wy[1:3,]

## ---- eval = FALSE------------------------------------------------------------
#  dt_wy <- calculate.arr(dt_wy, 2000, 2010) # ARR
#  dt_wy <- calculate.arr(dt_wy, 2010, 2020) # ARR
#  dt_wy <- calculate.pd(dt_wy, 2000, 2020) # percentage decline
#  dt_wy[Quantile == "Median" & Shortind == "NMR", ][1:3,]

## ---- eval = FALSE------------------------------------------------------------
#  dir_IGME_input <- get.IGMEinput.dir(2022)
#  dir_U5MR   <- get.dir_U5MR(dir_IGME_input)
#  dir_IMR    <- get.dir_IMR(dir_IGME_input)
#  dir_NMR    <- get.dir_NMR(y5 = TRUE) # either 5-year or not
#  dir_NMR    <- get.dir_NMR(y5 = FALSE) # either 5-year or not
#  dir_gender <- get.dir_gender(plotting = TRUE) # either dataset for plotting or modeling
#  dir_gender <- get.dir_gender(plotting = FALSE) # either dataset for plotting or modeling

## -----------------------------------------------------------------------------
# `dplyr::recode` can do the same and is about 10x faster:
dplyr::recode(c("a","b", "c"), !!!c("a" = "label 1", "b" = "label 2"))

get.match(c("a","b", "c"), new_list = list("a" = "label 1", "b" = "label 2"))

## -----------------------------------------------------------------------------
str(UNICEF_colors)

