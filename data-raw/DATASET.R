## code to prepare `DATASET` dataset goes here
shortind_indicator <- list(
    "U5MR"   =   "Under-five mortality rate",
    "IMR"    =   "Infant mortality rate",
    "NMR"    =   "Neonatal mortality rate",
    "MR1t59" =   "Mortality rate age 1-59 months", # added Sep.2021
    "CMR"    =   "Child mortality rate age 1-4",
    "SBR"    =   "Stillbirth rate",

    "Under.five.deaths"    =  "Under-five deaths",
    "Infant.deaths"        =  "Infant deaths",
    "Neonatal.deaths"      =  "Neonatal deaths",
    "Deaths.1to59"         =  "Deaths age 1 to 59 months",
    "Child.deaths.age1to4" =  "Child deaths age 1 to 4",
    "SB"                   =  "Stillbirths",

    "10q5"   =   "Mortality rate age 5-14",
    "5q5"    =   "Mortality rate age 5-9",
    "5q10"   =   "Mortality rate age 10-14",
    "10q15"  =   "Mortality rate age 15-24",
    "5q15"   =   "Mortality rate age 15-19",
    "5q20"   =   "Mortality rate age 20-24",
    "X20q5"  =   "Mortality rate age 5-24",
    "X10q10" =   "Mortality rate age 10-19",

    "Deaths.age.5to14"     =  "Deaths aged 5 to 14",
    "Deaths.age.5to9"      =  "Deaths aged 5 to 9",
    "Deaths.age.10to14"    =  "Deaths aged 10 to 14",
    "Deaths.age.15to24"    =  "Deaths aged 15 to 24",
    "Deaths.age.15to19"    =  "Deaths aged 15 to 19",
    "Deaths.age.20to24"    =  "Deaths aged 20 to 24",
    "Deaths.age.5to24"     =  "Deaths aged 5 to 24",
    "Deaths.age.10to19"    =  "Deaths aged 10 to 19"
  )

usethis::use_data(shortind_indicator, overwrite = TRUE)
