

set.seed(1234)
options(digits = 3)

# Stata Engine


knitr::opts_chunk$set(
  # comment = "#>",
  # comment=NA,
  warning = FALSE,
  collapse = TRUE,
  cache = TRUE,
  out.width = "70%",
  fig.align = 'center',
  fig.width = 6,
  fig.asp = 0.618,
  # 1 / phi
  fig.show = "hold",
  engine.path = list(stata = stata_eng)
)

options(dplyr.print_min = 6, dplyr.print_max = 6)


# install and load packages that have not been installed before
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
    "formattable"
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


knitr::opts_chunk$set(tidy = FALSE, cache.extra = packageVersion('tufte'))
options(htmltools.dir.version = FALSE)
