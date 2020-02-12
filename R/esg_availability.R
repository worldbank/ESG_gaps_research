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


# Plot Heatmap
g1 <- ggplot(d1, aes( x = date,
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
pg1 <- ggplotly(g1, tooltip = "text")




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
    ggplotly(g1a, tooltip = "text")
    }
}

