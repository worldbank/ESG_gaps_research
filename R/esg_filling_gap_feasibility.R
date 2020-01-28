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


# find max year per indicator/country
x %>%
  group_by(indicatorID, iso3c) %>%
  summarise(mxy = max(date))  %>%  # max year avaiable
  mutate()


