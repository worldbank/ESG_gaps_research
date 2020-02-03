
#----------------------------------------------------------
# install and load packages
#----------------------------------------------------------

#https://www-huber.embl.de/users/msmith/msmbstyle/index.html
if (!("msmbstyle"  %in% installed.packages()[, "Package"])) {
  remotes::install_github("grimbough/msmbstyle")
}

if (!("ggtext"  %in% installed.packages()[, "Package"])) {
  devtools::install_github("clauswilke/ggtext")
}


pkg <-
  c(
    "tidyverse",
    "readr",
    "png",
    "grid",
    "DiagrammeR",
    "kableExtra",
    "plotly",
    "zoo",
    "wbstats",
    "hrbrthemes",
    "viridis",
    "ggdendro",
    "scales",
    "formattable",
    "tufte",
    "povcalnetR",
    "rworldmap",
    "countrycode",
    "scales",
    "tables",
    "paletteer",
    "maps",
    "lubridate",
    "ggmosaic",
    "ggiraph",
    "ggtext",
    "extrafont",
    "bookdown",
    "DT",
    "tidytext",
    "patchwork"
  )
new.pkg <-
  pkg[!(pkg %in% installed.packages()[, "Package"])] # check installed packages
load.pkg <-
  pkg[!(pkg %in% loadedNamespaces())]              # check loaded packages

if (length(new.pkg)) {
  install.packages(new.pkg)     # Install missing packages
}

if (length(load.pkg)) {
  inst = lapply(load.pkg, library, character.only = TRUE) # load all packages
}


#----------------------------------------------------------
#   parameters
#----------------------------------------------------------

set.seed(1234)
options(digits = 3)


#----------------------------------------------------------
#   Knit options
#----------------------------------------------------------



knitr::opts_chunk$set(
  warning = FALSE,
  collapse = TRUE,
  message = FALSE,
  echo = FALSE
)


