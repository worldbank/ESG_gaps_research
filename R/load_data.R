# ==================================================
# project:       load data bases
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    sep 26 2019
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             Rdata
# ==================================================

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
    "formattable",
    "tufte",
    "povcalnetR",
    "rworldmap",
    "countrycode",
    "scales"
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
#   Load and prepare data
#----------------------------------------------------------

# Blend and regions vector
ic <- wb_cachelist$countries %>%
  filter(!(lending  %in% c("Aggregates", "Blend")) ) %>%
  select(iso3c)

ci_name <- wb_cachelist$countries %>%
  filter(!(lending  %in% c("Aggregates", "Blend")) ) %>%
  select(country, iso3c, region)

# Indicators codes and names
inames <- wb_cachelist$indicators[, c(1,2)]
names(inames)[2] <- "ind_name"



codes <- read_csv("data/esg_codes.csv")

mtd <- read_csv("data/esg_metadata.csv")

# x <- wb(indicator = codes$code)
# x <- as_tibble(x)

# cleaning
#x$date <-  as.numeric(x$date)
# exclude blends and regions
# x <- x[x$iso3c  %in% ic$iso3c, ]
#save(x, file = "data/ESG_wdi.RData")
load(file = "data/ESG_wdi.RData")

