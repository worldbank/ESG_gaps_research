source("R/utils.R")
source("R/esg_variability_utils.R")
#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
library("viridis")


#----------------------------------------------------------
#   Variability of ESG indicators
#----------------------------------------------------------

#--------- Calculations
# x object is created by load_data.R
esg_wdi <-  inner_join(x, select(mtd, cetsid, no_gap), by=c("indicatorID"="cetsid")) %>%
  mutate(indicatorID=ifelse(indicatorID=='WBL', 'WBL.', indicatorID)) %>% # needs a '.' to be included below
  # filter(no_gap==0) %>%
  select(-iso2c, -country, -indicator) %>% # keep important variables
  filter(date >= 2000, date <= 2018, !is.na(iso3c)) %>% # filter older years
  distinct(iso3c, date, indicatorID, .keep_all = TRUE) %>% #  Remove duplicates
  arrange(iso3c, date) %>%
  spread(indicatorID, value)       # Convert in wide form


# Indicators ID
ind_ID <- x %>%
  mutate(indicatorID=ifelse(indicatorID=='WBL', 'WBL.', indicatorID)) %>% # needs a '.' to be included below
  distinct(indicator, indicatorID) %>%
  rename(ind_name = indicator ,
         indicator = indicatorID)

# Scale variablese
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
  geom_histogram(aes(y = ..density..),    # histogram chart
                 alpha = 0.8,
                 position = 'identity',
                 bins = 15) +
  viridis::scale_fill_viridis(discrete=TRUE) +
  viridis::scale_color_viridis(discrete=TRUE) +
  hrbrthemes::theme_ipsum() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.grid = element_blank()
  ) +
  xlab("Mean Coefficient of variation") +
  ylab("K-density") +
  geom_density(alpha = .2, fill = "#CCFFFF") +     # add density char
  geom_vline(aes(xintercept = mean(cv, na.rm = TRUE)),
             color = "#FF3333", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = q_cv[1]),
             color = "#3399FF", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = q_cv[2]),
             color = "#3399FF", linetype = "dashed", size = 1)

#--------- Breaking down histogram
mean_cv <- var_ind %>%
  summarise(mean(cv, na.rm = TRUE))

q_cv <- quantile(var_ind$cv, na.rm = TRUE,
                 probs = c(0.05, 0.95))

# number of countries
n_c <- var_ind %>%
  filter(!is.na(cv)) %>%
  summarise(ind_n  = n_distinct(indicator))

n_c_low <- var_ind %>%
            filter(cv < q_cv[1])  %>%
            summarise(ind_n  = n_distinct(indicator))

n_c_high <- var_ind %>%
            filter(cv > q_cv[2])  %>%
            summarise(ind_n  = n_distinct(indicator))

n_c_mid <- var_ind %>%
  filter(cv >= q_cv[1], cv <= q_cv[2])  %>%
  summarise(ind_n  = n_distinct(indicator))


# shares
s_c_low <- n_c_low/n_c
s_c_high <- n_c_high/n_c
s_c_mid <- n_c_mid/n_c

#--------- table with Low and High CV

t_lh <- var_ind %>%
  #rename(indicator_id = indicator) %>%
  inner_join(inames, by = c("indicator" = "indicator_id")) %>%
  #rename(ind_name = indicator) %>%
  mutate(class_cv = case_when(
    cv <= q_cv[1] ~ "Low",
    cv >= q_cv[2] ~ "High"
  )) %>%
  filter(!is.na(class_cv)) %>%
  select(class_cv, ind_name)


# CV vay country and indicator

oc <- var_country %>%
  filter(!is.na(cv), cv > 0) %>%
  group_by(iso3c) %>%
  summarise(mcv = mean(cv, na.rm = TRUE)) %>%
  arrange(mcv, iso3c) %>%
  transmute(iso3c = iso3c,
            iso = factor(iso3c, levels = unique(iso3c)))

oi <- var_country %>%
  filter(!is.na(cv), cv > 0) %>%
  group_by(indicator) %>%
  summarise(mcv = mean(cv, na.rm = TRUE)) %>%
  arrange(mcv, indicator) %>%
  transmute(ind = factor(indicator, levels = unique(indicator)),
            indicator = indicator)

g2 <- var_country %>%
  filter(!is.na(cv), cv >= 0) %>%
  inner_join(oc) %>%
  inner_join(oi) %>%
  inner_join(inames, by = c("indicator" = "indicator_id")) %>%
  mutate(text = paste0("Country: ", iso, "\n",
                       "Indicator: ", ind_name, "\n",
                       "value: ", round(cv, 2), "\n")) %>%
  ggplot(aes(x = iso,
             y = ind,
             fill = cv,
             text = text)) +
  geom_tile() +
  scale_fill_viridis_c(option = "A", alpha = .8, direction = -1) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = rel(0.4),
                                   colour = "grey50"))
  # ggtitle(label = "Variability of ESG indicators by country")

pg2 <- plotly::ggplotly(g2, tooltip = "text")


# country with highest CV per indicators
max_cty_ind <- var_country %>%
  group_by(indicator) %>%
  filter(!is.na(cv), cv > 0) %>%
  left_join(ci_name) %>%
  summarise(rank = which.max(cv),
            max_cty = country[rank]) %>%
  select(-rank)


# country with lowest CV per indicators
min_cty_ind <- var_country %>%
  group_by(indicator) %>%
  filter(!is.na(cv), cv > 0) %>%
  left_join(ci_name) %>%
  summarise(rank = which.min(cv),
            min_cty = country[rank]) %>%
  select(-rank)


# Indicators with highest variability range across countries
diff_ind <- var_country %>%
  group_by(indicator) %>%
  filter(!is.na(cv), cv > 0) %>%  # get rid of NA and zero (old NAs)
  summarise(                    # get diff of max and min
    min = min(cv),
    max = max(cv),
    diff = max - min
  ) %>%
  left_join(ind_ID) %>%        # merge indicator name
  left_join(max_cty_ind) %>%   # merge country with highest CV
  left_join(min_cty_ind) %>%   # merge country with lowest CV
  arrange(-diff) %>%
  select(ind_name, min, max, diff, max_cty, min_cty)

high_diff_ind <- diff_ind %>%
  slice(1:6) %>%   # select 10 greatest diffs
  transmute(
    ind_name = ind_name,
    mxc = paste0(max_cty, "(", round(max, 2), ")"),
    mnc = paste0(min_cty, "(", round(min, 2), ")")
  )

# container of DataTable DT
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Indicator'),
      th(colspan = 2, 'Highest Variability'),
      th(colspan = 2, 'Lowest Variability')
    ),
    tr(
      lapply(rep(c('Country', 'CV'), 2), th),
      th("Diff")
    )
  )
))



q_diff <- quantile(diff_ind$diff, na.rm = TRUE,
         probs = c(0.05, 0.95))

g_diff <- ggplot(data = diff_ind ,
       aes(x = diff)) +
  geom_histogram(aes(y = ..density..),
                 alpha = 0.8,
                 position = 'identity',
                 bins = 15) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  hrbrthemes::theme_ipsum() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.grid = element_blank()
  ) +
  xlab("Range of coefficient of variation of indicators across countries") +
  ylab("K-density") +
  geom_density(alpha = .2, fill = "#CCFFFF") +     # add density char
  geom_vline(aes(xintercept = mean(diff, na.rm = TRUE)),
             color = "#FF3333", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = q_diff[1]),
             color = "#3399FF", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = q_diff[2]),
             color = "#3399FF", linetype = "dashed", size = 1)



# table with ESG indicators with lowest and highest volatility
lvarn <- paste0("Low volatility (CV <= ", round(q_cv[1],2),")")
hvarn <- paste0("High volatility (CV >= ", round(q_cv[2],2),")")

t_lh2 <- t_lh %>% group_by(class_cv) %>%
  mutate(n = row_number()) %>%
  spread(class_cv, ind_name) %>%
  mutate(High = replace_na(High, replace = "")) %>%
  transmute(
    Low  =  Low,
    High =  High
  ) %>%
  mutate(
    Low = replace_na(Low, ""),
    High = replace_na(High, "")
  ) %>%
  # mutate_all(~str_replace(., "\\$", "&#36;"))
  mutate_all(~str_replace(., "\\$", "\\\\$"))


# Create dataset for interactive viz -------------------------------------------

# Load data
# cvs <- feather::read_feather('./data/ESG_cv.feather') %>%
#   arrange(cetsid) %>%
#   pivot_longer(cols = -cetsid, names_to = "iso3", values_to = "cv")

mrv <- create_mrv(df = x)

n_indicators <- length(unique(mrv$indicatorID))
n_countries <- length(unique(mrv$iso3))

# Create multiple dfs
years_vector <- seq(from = 2010, to = 2018, by = 1)
cv_max_vector <- seq(from = 0.1, to = 1, by = .05)
years_to_impute_vector <- seq(from = 1, to = 3, by = 1)

dfs_list <- vector(mode = "list", length = length(years_vector) * length(cv_max_vector) * length(years_to_impute_vector))

i <- 1
for (year in seq_along(years_vector)) {
  for (cv_max in seq_along(cv_max_vector)) {
    for (nyears in seq_along(years_to_impute_vector)) {

      out <- create_baseline_imputed_df(mrv = mrv,
                                        cvs = var_country,
                                        cv_max = cv_max_vector[cv_max],
                                        years_to_impute = years_to_impute_vector[nyears],
                                        year_select = years_vector[year],
                                        n_countries = n_countries)

      out$year <- years_vector[year]
      out$cv_max <- cv_max_vector[cv_max]
      out$years_to_impute <- years_to_impute_vector[nyears]
      #print(head(out))

      dfs_list[[i]] <- out

      i <- i + 1
    }
  }
  #print(i)
}

imputed_dfs <- dplyr::bind_rows(dfs_list)
# Add missings explicitly
# This is necessery to keep the width of the column chart constant
esg_lkup <- imputed_dfs %>%
  dplyr::select(indicatorID, year, cv_max, years_to_impute) %>%
  dplyr::distinct() %>%
  tidyr::expand(indicatorID, year, cv_max, years_to_impute)

imputed_dfs <- esg_lkup %>%
  dplyr::left_join(imputed_dfs)


# readr::write_rds(imputed_dfs, path = "./data/esg_imputed_dfs.rds")
# pins::board_register_rsconnect(key    = Sys.getenv("connect_key_ext"),
#                                server = Sys.getenv("connect_ext_server2"))
# pins::pin(imputed_dfs, "esg_imputed_dfs", board = "rsconnect")
