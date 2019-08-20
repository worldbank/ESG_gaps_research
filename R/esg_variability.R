#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library(tidyverse)
library(wbstats)
library(data.table)
library(zoo)


#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------

# coefficient of variation
cv <- function(x) {
  mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE)
}


# Quartile coefficient of dispersion

qcd <- function(x) {
  q <- quantile(x, na.rm = TRUE)

  # Interquantile range
  a <- (q[[4]] - q[[2]])/2

  # Midhinge

  b <-  (q[[4]] + q[[2]])/2


  # qcd
  c <- a/b
  return(c)
}


#----------------------------------------------------------
#   Load and prepare data
#----------------------------------------------------------

codes <- read_csv("data/esg_codes.csv")

x <- wb(indicator = codes$code)
x <- as_tibble(x)

# save(x, file = "data/ESG_wdi.RData")


esg <-  x %>%
  select(-iso2c, -country, -indicator) %>% # keep important variables
  filter(date > 1990, date <= 2019) %>% # filter older years
  tibble::rowid_to_column() %>%
  spread(indicatorID, value)       # Convert in wide form

#----------------------------------------------------------
#   Variability analysis
#----------------------------------------------------------


# using zoo package

var_country <- esg %>%
  group_by(iso3c) %>%   # calculations done by country
  mutate_at(vars(matches("\\.")),na.approx, na.rm = FALSE) %>% # Interpolate data (linear)
  summarise_at(vars(matches("\\.")), list(cv = cv, qcd = qcd)) %>%  # variability
  gather("indicators", "value", -iso3c) %>%   # long form
  separate(indicators, c("indicator", "measure"), sep = "_") %>%  # split variable
  spread(measure, value ) %>%  # wide form
  ungroup()

var_ind <-  var_country %>%
  group_by(indicator) %>%
  summarise_at(vars("cv", qcd), mean, na.rm = TRUE)




#----------------------------------------------------------
#   For testing only (Dd NOT Run from this point)
#----------------------------------------------------------

esg <-  x %>%
  select(-iso2c, -country, -indicator) %>%
  filter(iso3c  %in% c("COL", "MEX"), date > 1990, date < 2020) %>%
  spread(indicatorID, value) %>%
  select(iso3c, date, GB.XPD.RSDV.GD.ZS,  EN.ATM.CO2E.KT)
