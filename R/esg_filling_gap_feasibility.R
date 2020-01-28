# ==================================================
# project:       Feasibility of extrapolation based on volatiliy and coverage
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-01-28
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             chart
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("plotly")

source("_common.R")
source("R/load_data.R")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#----------------------------------------------------------
# Parameters
#----------------------------------------------------------


th_cg = .5       # threshold of coverage
th_cv = .3       # Threshold of coefficient of variations
tg_year = 2018   # target year
mx_year = 2018   # Max year available option2: as.numeric(format(Sys.Date(), "%Y")) - 1


# find max year per indicator/country
x %>%
  group_by(indicatorID, indicator,  iso3c) %>%
  summarise(mxy = max(date, na.rm = TRUE))  %>%  # max year avaiable
  mutate(lst_y = if_else(mxy  %in% c(tg_year:mx_year), 1, 0)) %>% # is last year aviable?
  group_by(indicatorID, indicator) %>%
  summarise(lst_yd = mean(lst_y, na.rm = TRUE)) %>% # last year density
  arrange(-lst_yd)


