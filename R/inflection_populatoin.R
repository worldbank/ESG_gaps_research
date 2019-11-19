# ==================================================
# project:       find the inflection point in population distribution
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-11-19
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             output
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("wbstats")
#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#----------------------------------------------------------
#
#----------------------------------------------------------
# Blend and regions vector
ic <- wb_cachelist$countries %>%
  filter(!(lending  %in% c("Aggregates", "Blend")) ) %>%
  select(iso3c)

pop <- as_tibble(wb(indicator = "SP.POP.TOTL")) %>%
  filter(date == 2018,
         iso3c  %in% ic$iso3c) %>%
  select(iso3c, value)


