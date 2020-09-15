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


#----------------------------------------------------------
#   Load and prepare data
#----------------------------------------------------------
library(tidyverse)
library(wbstats)
# Blend and regions vector
ic <- wb_cachelist$countries %>%
  filter(!(lending_type  == "Aggregates") ) %>%
  select(iso3c)

ci_name <- wb_cachelist$countries %>%
  filter(!(lending_type  == "Aggregates") ) %>%
  select(country, iso3c, region)

# Indicators codes and names
inames <- wb_cachelist$indicators[, c(1,2)]
names(inames)[2] <- "ind_name"



codes <- read_csv("data/esg_codes.csv")

meta_csv <- "data/esg_metadata.csv"
guess <- guess_encoding(meta_csv)

# mtd <- read_csv("data/esg_metadata.csv",
#                 locale = readr::locale(encoding = guess[[1,1]]))

reticulate::source_python("python/esg_loader.py")
mtd <- load_metadata(metafile_path = "./data/esg_metadata.csv",
                     datafile_path = "./data/ESG_wdi.feather")
# FIX classes
mtd[] <- map_if(mtd, is.list, as.character)
mtd[] <- map_if(mtd, is.logical, as.numeric)
mtd[, c("release_gap", "metadata_level", "data_volatility")] <- lapply(mtd[, c("release_gap", "metadata_level", "data_volatility")], as.logical)
# FIX NaN
mtd[] <- lapply(mtd, function(x) {
  out <- x
  out[x == "NaN"] <- NA

  return(out)
})
# FIX extra white spaces issues
mtd[] <- map_if(mtd, is.character, stringr::str_trim)


#----------------------------------------------------------
#   Initial parameters
#----------------------------------------------------------

first_year <- 2000
last_year  <- 2019



# x <- wb(indicator = codes$code)
# x <- as_tibble(x)

# cleaning
# x$date <-  as.numeric(x$date)
# exclude blends and regions
# x <- x[x$iso3c  %in% ic$iso3c, ]
# save(x, file = "data/ESG_wdi.RData")
# write.csv(x, file = "data/ESG_wdi.csv", row.names = FALSE)
# feather::write_feather(x, "data/ESG_wdi.feather")
# load(file = "data/ESG_wdi.RData")
x <- feather::read_feather("data/ESG_wdi.feather") %>%
  filter(date >= first_year, date <= last_year)


#----------------------------------------------------------
#   Make sure No_gap is the same in all databases
#   translation of section in esg_loader.py
#----------------------------------------------------------


# calculate no_pop, defined as any indicator with a value for 2018 or later for 90%+ of economies
min_economies <-  length(unique(x$iso3c)) * 0.9

# dataframe of indicators with counts of countries with at least one MRV
mrv_series <- x %>%
  filter(date >= 2018) %>%
  group_by(indicatorID, iso3c) %>%
  count() %>%
  group_by(indicatorID) %>%
  count()

no_gaps <- mrv_series %>%
  filter(n >= min_economies) %>%
  select(cetsid = indicatorID,
         n_no_gaps = n) %>%
  mutate(
    no_gap = 1
  )


# set no_gap for indicators in the previous dataframe


mtd <- mtd %>%
  left_join(no_gaps, by = "cetsid")  %>%
  mutate(
    no_gap.y = if_else(is.na(no_gap.y), 0, no_gap.y),
    no_gap   = no_gap.y
  ) %>%
  select(-c(no_gap.y, no_gap.x))



