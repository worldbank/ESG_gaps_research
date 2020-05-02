source("R/utils.R")
#----------------------------------------------------------
#   Number of countries per indicator over time"
#----------------------------------------------------------

#--------- heatmap indicators years and No. of countries

d1 <- x %>%
  filter(date >= 2000, date <= 2018) %>%
  group_by(indicatorID,indicator, date) %>%
  count(date) %>%
  mutate(text = paste0("Indicator: ", indicator, "\n",
                       "Indicator ID: ", indicatorID, "\n",
                       "Year: ", date, "\n",
                       "No. countries: ", n, "\n"))

# Sort indicators from most data points to less overall
o <- d1 %>%
  group_by(indicatorID) %>%
  summarise(n2 = sum(n)) %>%
  arrange(n2) %>%
  mutate(ind = factor(indicatorID, levels = unique(indicatorID)))

d1 <- inner_join(d1, o)


d2 <- x %>%
  filter(date >= 2000, date <= 2018) %>%
  group_by(indicatorID,indicator, date) %>%
  summarise(n = n_distinct(iso3c)) %>%
  group_by(indicatorID,indicator)  %>%
  summarise(mean = mean(n, na.rm = TRUE)) %>%
  ungroup()

#----------------------------------------------------------
#   Average growth of countries per year in each indicator
#----------------------------------------------------------

lmdi <- x %>%  # improvement
  filter(date >= 2000, date <= 2018) %>%
  group_by(indicatorID,indicator, date) %>%
  summarise(n = n_distinct(iso3c)) %>%
  ungroup() %>%
  nest(data = -c(indicator, indicatorID)) %>%
  mutate(
    fit  = purrr::map(data, ~lm(n ~ date, data = .)),  # regression
    beta = purrr::map(fit, ~broom::tidy(.)[["estimate"]][2])  # extract beta
  ) %>%
  unnest(beta) %>%
  select(indicatorID, indicator, beta) %>%
  arrange(-beta)


#----------------------------------------------------------
#   Indicators stable over time
#----------------------------------------------------------

fillin <- expand_grid(
  date        = c(2000:2018),
  indicatorID = unique(x$indicatorID)
  ) %>%
  inner_join(
    tibble(
      indicatorID  = unique(x$indicatorID),
      indicator    = unique(x$indicator)
    )
  )

si <- x %>%
  filter(date >= 2000, date <= 2018) %>%
  group_by(indicatorID,indicator, date) %>%
  summarise(nc = n_distinct(iso3c))   %>%
  full_join(fillin) %>%
  arrange(indicatorID, date) %>%
  mutate(
    nc = if_else(is.na(nc), 0L, nc)
  ) %>%
  group_by(indicatorID, indicator) %>%
  summarise(
    mean = mean(nc, na.rm = TRUE),
    sd   = sd(nc, na.rm = TRUE)
    ) %>%
  filter(mean > 0) %>%
  ungroup() %>%
  arrange(sd, -mean)


#----------------------------------------------------------
#   Sudden decline
#----------------------------------------------------------

sdd <- x %>%
  filter(date >= 2000, date <= 2018) %>%
  group_by(indicatorID,indicator, date) %>%
  summarise(nc = n_distinct(iso3c))   %>%
  full_join(fillin,
            by = c("indicatorID", "indicator", "date")
            ) %>%
  arrange(indicatorID, date) %>%
  mutate(
    nc = if_else(is.na(nc), 0L, nc)
  ) %>%
  group_by(indicatorID, indicator) %>%
  mutate(
    sdi = nc - lag(nc)
  ) %>%
  group_by(indicatorID) %>%
  filter(sdi == min(sdi, na.rm = TRUE)) %>%
  filter(sdi < 0) %>%
  filter(date == max(date)) %>%
  mutate(sdi = sdi*-1) %>%
  arrange(-sdi) %>%
  select(indicatorID, indicator, date, sdi)


#----------------------------------------------------------
#   Charts
#----------------------------------------------------------


# Plot Heatmap
g1 <- ggplot(data = filter(d1, date >= 2000, date <= 2018),
             aes( x = date,
                  y = ind,
                  fill = n,
                  text = text)) +
  geom_tile() +
  scale_fill_viridis_c(option = "A", alpha = .8,
                       limits = c(0, 220),
                       breaks = c(0, 50, 100, 150, 200)) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(2000:2018),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(size = rel(0.8),
                                   angle = 330,
                                   hjust = 0,
                                   colour = "grey50"),
        axis.text.y = element_text(size = rel(0.5),
                                   colour = "grey50")) +
  ggtitle(label = "Number of countries per indicator over time")
#g1
# make it interactive
pg1 <- plotly::ggplotly(g1, tooltip = "text")




#----------------------------------------------------------
#   heatmap by explantion
#----------------------------------------------------------


d1a <- d1 %>%
  filter(date >= 2000, date <= 2018) %>%
  inner_join(select(mtd, cetsid, matches("^expl")),
             by = c("indicatorID" = "cetsid")) %>%
  ungroup() %>%
  mutate(ideal = if_else(rowSums(select(., matches("^expl"))) == 0,
                         1, 0))


hm_expl <- function(x, expl,
                    label = NULL) {
  expl <- enquo(expl)

  if (length(label) > 0) {
    labelf <-  paste0("Number of countries over time per indicator (",
                     label, ")")
  } else {
    labelf <- "Number of countries over time per indicator"
  }

  y <- filter(x, !!expl == 1)

  if (dim(y)[[1]] == 0) {
    invisible("")
  } else {
    g1a <- ggplot(data = y,
                  aes(text = text)) +
      geom_tile(aes( x = date,
                     y = ind,
                     fill = n)) +
      scale_fill_viridis_c(option = "A", alpha = .8,
                           limits = c(0, 220),
                           breaks = c(0, 50, 100, 150, 200)) +
      labs(x = "", y = "") +
      scale_x_continuous(breaks = c(2000:2018),
                         expand = c(0,0)) +
      theme(axis.text.x = element_text(size = rel(0.8),
                                       angle = 330,
                                       hjust = 0,
                                       colour = "grey50"),
            axis.text.y = element_text(size = rel(0.5),
                                       colour = "grey50")) +
      ggtitle(label = labelf)
    plotly::ggplotly(g1a, tooltip = "text")
    }
}

