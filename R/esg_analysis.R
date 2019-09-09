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
library("zoo")
library("plotly")
library("wbstats")
library("hrbrthemes")
library("viridis")
library("ggdendro")


#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------



#----------------------------------------------------------
#   Load and prepare data
#----------------------------------------------------------

codes <- read_csv("data/esg_codes.csv")

# x <- wb(indicator = codes$code)
# x <- as_tibble(x)

# save(x, file = "data/ESG_wdi.RData")
load(file = "data/ESG_wdi.RData")

#----------------------------------------------------------
#   Charts
#----------------------------------------------------------



#--------- heatmap indicators years and No. of countries

d1 <- x %>%
  group_by(indicatorID,indicator, date) %>%
  filter(date >= 1980, date <= 2018) %>%
  count(date) %>%
  mutate(text = paste0("date: ", date, "\n",
                       "Indicator: ", indicator, "\n",
                       "No. countries: ", n, "\n"))

o <- d1 %>%
  group_by(indicatorID) %>%
  summarise(n2 = sum(n)) %>%
  arrange(n2) %>%
  mutate(ind = factor(indicatorID, levels = unique(indicatorID)))

d1 <- inner_join(d1, o)

g1 <- ggplot(d1, aes( x = date,
                      y = ind,
                      fill = n,
                      text = text)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdPu") +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(axis.text.x = element_text(size = rel(0.8),
                                 angle = 330,
                                 hjust = 0,
                                 colour = "grey50"),
        axis.text.y = element_text(size = rel(0.8),
                                   colour = "grey50")) +
ggtitle(label = "No. of countries per indicator over time")

ggplotly(g1, tooltip = "text")
