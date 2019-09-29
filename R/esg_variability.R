
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



#--------- histogram of CV

# Mean Coefficient of variation for each indicator
g_cv <- var_ind %>% ggplot(aes(x = cv)) +
  geom_histogram(aes(y = ..density..),
                 alpha = 0.8,
                 position = 'identity',
                 bins = 15) +
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



#--------- Breaking down histogram
mean_cv <- var_ind %>% summarise(mean(cv, na.rm = TRUE))

# number of countries
n_c <- summarise(var_ind, ind_n  = n_distinct(indicator))
n_c_low <- var_ind %>%
            filter(cv < 1)  %>%
            summarise(ind_n  = n_distinct(indicator))

n_c_high <- var_ind %>%
            filter(cv > 2)  %>%
            summarise(ind_n  = n_distinct(indicator))

n_c_mid <- var_ind %>%
  filter(cv >= 1, cv <= 2)  %>%
  summarise(ind_n  = n_distinct(indicator))


# shares
s_c_low <- n_c_low/n_c
s_c_high <- n_c_high/n_c
s_c_mid <- n_c_mid/n_c

#--------- table with Low and High CV

t_lh <- var_ind %>%
  inner_join(inames, by = c("indicator" = "indicatorID")) %>%
  mutate(class_cv = case_when(
    cv <= 1 ~ "Low",
    cv >= 2 ~ "High"
  )) %>%
  filter(!is.na(class_cv)) %>%
  select(class_cv, ind_name)

a <- paste(t_lh$ind_name[t_lh$class_cv == "Low"],  collapse = "<br>- ")
b <- paste(t_lh$ind_name[t_lh$class_cv == "High"], collapse = "<br>- ")

t_lh2 <- tibble(
  `Low volatility (CV <= 1)`  = paste0("- ",a),
  `High volatility (CV >= 2)` = paste0("- ",b)
)

t_lh3 <- knitr::kable(t_lh2, escape = FALSE,
             caption = "ESG indicators with lowest and highest volatility") %>%
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed",
                                      "responsive"),
                full_width = F) %>%
  column_spec(1, width = "20em", border_right = T) %>%
  column_spec(2, width = "20em")

t_lh4 <- t_lh %>% group_by(class_cv) %>%
  mutate(n = row_number()) %>%
  spread(class_cv, ind_name) %>%
  mutate(High = paste0("- ", High),
         Low  = paste0("- ", Low),
         High = str_replace(High, '- NA', '')) %>%
  transmute(
    `Low volatility (CV <= 1)`  =  Low,
    `High volatility (CV >= 2)` =  High
  )


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

