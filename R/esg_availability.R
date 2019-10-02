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


d2 <- x %>%
  filter(date >= 1980, date <= 2018) %>%
  group_by(indicatorID,indicator, date) %>%
  summarise(n = n_distinct(iso3c)) %>%
  group_by(indicatorID,indicator)  %>%
  summarise(mean = mean(n, na.rm = TRUE)) %>%
  ungroup()



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

