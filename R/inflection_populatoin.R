# ==================================================
# project:       find the inflection point in population distribution
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-11-19
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             output
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("wbstats")
library("plotly")
library("paletteer")
#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#----------------------------------------------------------
#
#----------------------------------------------------------
# Blend and regions vector
ic <- wb_cachelist$countries %>%
  filter(!(lending  %in% c("Aggregates", "Blend")) ) %>%
  select(iso3c) %>% pull()

pop <- as_tibble(wb(indicator = "SP.POP.TOTL")) %>%
  filter(date == 2018,
         iso3c  %in% ic) %>%
  select(iso3c, value) %>%
  arrange(value) %>%
  filter(value < 1000000) %>%
  mutate(
    diff = c(NA, diff(value)),
    perc = c(diff(value), NA)/value,
    population = value
  )


pl <- paletteer_d(package = "RColorBrewer", palette = "Set1")
scales::show_col(pl)

ggplot(data = pop, aes(x = population, y = diff)) +
  geom_point(
    aes(colour = cut(population, c(0, 1.2e5, 1.6e5, 1e6))),
    alpha = 0.7, size = 2) +
  scale_color_manual(
    name = "Population/diff",
    values = pl,
    labels = c("Small Population", "Inflection", "Rest")
  ) +
  geom_vline(xintercept = 1.2e5,
             linetype = "dotted",
             color = pl[5]) +
  geom_hline(yintercept = 1.3e4,
             linetype = "dashed",
             color = pl[5]) +
  theme_classic()



ggplot(data = pop, aes(x = population, y = perc)) +
  geom_point(
    aes(colour = cut(population, c(0, 1.2e5, 1.6e5, 1e6))),
    alpha = 0.7, size = 2) +
  scale_color_manual(
    name = "Population/percentage",
    values = pl,
    labels = c("Small Population", "Inflection", "Rest")
  ) +
  geom_vline(xintercept = 1.2e5,
             linetype = "dotted",
             color = pl[5]) +
  theme_classic()
