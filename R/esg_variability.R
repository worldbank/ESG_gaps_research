#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library(tidyverse)
library(wbstats)
library(data.table)
library(zoo)
library(scales)


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

norm_prox <- function(x) {
  #p <- (x - mean(x, na.rm = TRUE)) / sd(x, TRUE)  # normalize
  p <- rescale(x, na.rm = TRUE)
  p <- na.approx(p, na.rm = FALSE)                # interpolate missings
  return(p)
}


#----------------------------------------------------------
#   Load and prepare data
#----------------------------------------------------------

# Blend and regions vector
ic <- wb_cachelist$countries %>%
  filter(!(lending  %in% c("Aggregates", "Blend")) ) %>%
  select(iso3c)

codes <- read_csv("data/esg_codes.csv")

# x <- wb(indicator = codes$code)
# x <- as_tibble(x)

# exclude blends and regions
# x <- x[x$iso3c  %in% ic$iso3c, ]
# save(x, file = "data/ESG_wdi.RData")
load(file = "data/ESG_wdi.RData")


esg <-  x %>%
  select(-iso2c, -country, -indicator) %>% # keep important variables
  filter(date > 1990, date <= 2019, !is.na(iso3c)) %>% # filter older years
  distinct(iso3c, date, indicatorID, .keep_all = TRUE) %>% #  Remove duplicates
  arrange(iso3c, date) %>%
  spread(indicatorID, value)       # Convert in wide form



#----------------------------------------------------------
#   Variability analysis
#----------------------------------------------------------

# Scale variables
esg_scaled <- esg %>%
  group_by(iso3c) %>%   # calculations done by country
  mutate_at(vars(matches("\\.")), norm_prox)  # normalize and Interpolate data (linear)

# Variability by country and indicator
var_country <- esg_scaled %>%
  summarise_at(vars(matches("\\.")), list(cv = cv, qcd = qcd)) %>%  # variability
  gather("indicators", "value", -iso3c) %>%   # long form
  separate(indicators, c("indicator", "measure"), sep = "_") %>%  # split variable
  spread(measure, value ) %>%  # wide form
  arrange(indicator, iso3c) %>%
  ungroup()

var_country$cv[var_country$cv == Inf | var_country$cv == -Inf] <- 0

# average of variability by indicator
var_ind <-  var_country %>%
  group_by(indicator) %>%
  summarise_at(vars("cv", "qcd"), mean, na.rm = TRUE) %>%
  arrange(cv, qcd, indicator)








