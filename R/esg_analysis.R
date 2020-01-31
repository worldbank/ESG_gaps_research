# ==================================================
# project:       Analysis of ESG data
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    09 sep 2019
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             charts
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("plotly")
library("ggiraph")
library("zoo")
library("wbstats")
library("hrbrthemes")
library("viridis")
library("ggdendro")
library("scales")
library("formattable")
library("knitr")
library("kableExtra")
library("extrafont")


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

  a <- (q[[4]] - q[[2]])/2 # Interquantile range
  b <-  (q[[4]] + q[[2]])/2 # Midhinge
  c <- a/b  # qcd
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

# Indicators codes and names
inames <- wb_cachelist$indicators[, c(1,2)]
names(inames)[2] <- "ind_name"



codes <- read_csv("data/esg_codes.csv")

# x <- wb(indicator = codes$code)
# x <- as_tibble(x)

# cleaning
#x$date <-  as.numeric(x$date)
# exclude blends and regions
# x <- x[x$iso3c  %in% ic$iso3c, ]
#save(x, file = "data/ESG_wdi.RData")
load(file = "data/ESG_wdi.RData")



#----------------------------------------------------------
#   Number of countries per indicator over time"
#----------------------------------------------------------

#--------- heatmap indicators years and No. of countries

d1 <- x %>%
  filter(date >= 1980, date <= 2018) %>%
  group_by(indicatorID,indicator, date) %>%
  count(date) %>%
  mutate(text = paste0("Year: ", date, "\n",
                       "Indicator: ", indicator, "\n",
                       "No. countries: ", n, "\n"))

# Sort indicators from most data points to less overall
o <- d1 %>%
  group_by(indicatorID) %>%
  summarise(n2 = sum(n)) %>%
  arrange(n2) %>%
  mutate(ind = factor(indicatorID, levels = unique(indicatorID)))

d1 <- inner_join(d1, o)

# Plot Heatmap
g1 <- ggplot(d1, aes( x = ind,
                      y = date,
                      fill = n,
                      text = text)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1) +
  labs(x = "", y = "") +
  scale_y_continuous(breaks = c(1980:2018),
                     expand = c(0,0))+
  theme(axis.text.y = element_text(size = rel(0.6),
                                   hjust = 0,
                                   colour = "grey50"),
        axis.text.x = element_blank()) +
  ggtitle(label = "Number of countries per indicator over time")
#g1
# make it interactive
pg1 <- ggplotly(g1, tooltip = "text")


#----------------------------------------------------------
#   Variability of ESG indicators
#----------------------------------------------------------

#--------- Calculations

esg_wdi <-  x %>%
  select(-iso2c, -country, -indicator) %>% # keep important variables
  filter(date > 1990, date <= 2019, !is.na(iso3c)) %>% # filter older years
  distinct(iso3c, date, indicatorID, .keep_all = TRUE) %>% #  Remove duplicates
  arrange(iso3c, date) %>%
  spread(indicatorID, value)       # Convert in wide form

# Scale variables
esg_scaled <- esg_wdi %>%
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



#--------- Charts

# Mean Coefficient of variation for each indicator
g_cv <- var_ind %>% ggplot(aes(x = cv)) +
  geom_histogram(aes(y = ..density..),
                 alpha = 0.8,
                 position = 'identity',
                 bins = 10) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.grid = element_blank()
  ) +
  xlab("Mean Coefficient of variation") +
  ylab("K-density")


mean_cv <- var_ind %>% summarise(mean(cv, na.rm = TRUE))



# CV vay country and indicator

oc <- var_country %>%
  group_by(iso3c) %>%
  summarise(mcv = mean(cv, na.rm = TRUE)) %>%
  arrange(mcv, iso3c) %>%
  transmute(iso3c = iso3c,
            iso = factor(iso3c, levels = unique(iso3c)))

oi <- var_country %>%
  group_by(indicator) %>%
  summarise(mcv = mean(cv, na.rm = TRUE)) %>%
  arrange(mcv, indicator) %>%
  transmute(ind = factor(indicator, levels = unique(indicator)),
            indicator = indicator)

g2 <- var_country %>%
  inner_join(oc) %>%
  inner_join(oi) %>%
  inner_join(inames, by = c("indicator" = "indicatorID")) %>%
  mutate(text = paste0("Country: ", iso, "\n",
                       "Indicator: ", ind_name, "\n",
                       "value: ", round(cv, 2), "\n")) %>%
  ggplot(aes(x = iso,
             y = ind,
             fill = cv,
             text = text)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral",
                       direction = -1) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = rel(0.4),
                                   colour = "grey50")) +
  ggtitle(label = "Variability of ESG indicators by country")

pg2 <- ggplotly(g2, tooltip = "text")



#----------------------------------------------------------
#   indicator is not in dataset after T year
#----------------------------------------------------------

iso_thr = .75  # Country threshols

d2 <- x %>%
  arrange(indicatorID, iso3c, date) %>%
  group_by(indicatorID) %>%
  mutate(gap = date - lag(date)) %>%  # create gap
  filter(date >= 2014, gap > 0 , gap < 2) %>% # filter data
  group_by(indicatorID, indicator, date) %>%
  mutate(n_country = n_distinct(iso3c)) %>%
  group_by(indicatorID) %>%
  mutate(max_year = max(date)) %>% # max yera available by indicator
  filter(date == max_year)


g3 <- d2 %>% summarise(mcountry = mean(n_country, na.rm = TRUE)) %>%
  ggplot(aes(x = mcountry)) +
  geom_histogram()


# Filter indicators with more than 75 of countries
max_nc = max(d2$n_country)  # xam number of countries in all indicators
d2 <- d2 %>%
  filter(n_country > round(max_nc*iso_thr)) %>%
  group_by(indicatorID) %>%
  summarise(year = mean(date, na.rm = TRUE))



d3 <- tibble(
  `last year` = as.character(2014:2019),
  indicators = c(
    paste(d2$indicatorID[d2$year == 2014], collapse = "\n"),
    paste(d2$indicatorID[d2$year == 2015], collapse = "\n"),
    paste(d2$indicatorID[d2$year == 2016], collapse = "\n"),
    paste(d2$indicatorID[d2$year == 2017], collapse = "\n"),
    paste(d2$indicatorID[d2$year == 2018], collapse = "\n"),
    paste(d2$indicatorID[d2$year == 2019], collapse = "\n")
  )
)

t_notavail <- kable(d3) %>%
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed",
                                      "responsive"),
                full_width = F,
                position = "left") %>%
  column_spec(1, width = "1em", bold = T, border_right = T) %>%
  column_spec(2, width = "20em", background = "#FFFFCC")


# indicators that are yearly and there is not data after 2017.


