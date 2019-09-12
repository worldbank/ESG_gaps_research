
#----------------------------------------------------------
#   library
#----------------------------------------------------------


#remotes::install_github("worldbank/povcalnetR")
library("povcalnetR")
library("tidyverse")
library("hrbrthemes")
library("plotly")
library("viridis")
library("lubridate")
library("zoo")

#----------------------------------------------------------
#   Download and Prepare Data
#----------------------------------------------------------

vars <- c("countrycode", "regioncode", "year", "datayear")

raw_df <-  povcalnet()

df <-  raw_df %>%
  filter(coveragetype == "N") %>%
  select(vars)

#d$yrs <- ymd(d$year, truncated = 2L)

df <- df %>%
  arrange(countrycode, year) %>%
  group_by(countrycode) %>%
  mutate(gap = year - lag(year), # difference betwen years
         decade = case_when(     # decade
           year  %in% 1970:1979 ~ "1970s",
           year  %in% 1980:1989 ~ "1980s",
           year  %in% 1990:1999 ~ "1990s",
           year  %in% 2000:2009 ~ "2000s",
           year  %in% 2010:2020 ~ "2010s",
         ),
         yrs = year(ymd(year, truncated = 2L))) %>%
  filter(gap > 0) %>%   # remove repeated years with income and consumption
  group_by(decade) %>%
  mutate(country_count = n_distinct(countrycode)) # No. of countries per decade



#----------------------------------------------------------
#   Heatmap of surveys per decada and production Gap
#----------------------------------------------------------

df_hh <- df %>%
  group_by(gap, decade) %>%
  summarise(n_country = n_distinct(countrycode)) %>%
  mutate(text = paste0("Decade: ", decade, "\n",
                       "Gap: ", gap, "\n",
                       "No. countries: ",n_country,"\n"))


g_hh <- ggplot(df_hh, aes(x = decade,
               y = gap,
               fill = n_country,
               text = text)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdPu") +
  theme_ipsum() +
  ggtitle(label = "No. of countries per survey gap")

pg_hh <- ggplotly(g_hh, tooltip = "text")


#----------------------------------------------------------
#   Production of surveys over time
#----------------------------------------------------------

df3 <- df %>%
    group_by(regioncode, yrs) %>%
    summarise(n_country = n_distinct(countrycode)) %>%
    mutate(ma_nc = rollmean(n_country, k = 3, fill = NA),
           text = paste0("Year: ", yrs, "\n",
                         "No. countries: ",ma_nc,"\n"))

g_hhp <- ggplot(df3, aes(x = yrs,
                       y = ma_nc,
                       fill = regioncode)) +
    geom_area(color = "black" , aes(fill=regioncode),
              position='stack', size=.5, alpha =.9) +
  ylab("Number of Surveys") +
  xlab("Year") +
  theme_ipsum()

pg_hhp <- ggplotly(g_hhp)



#----------------------------------------------------------
#   Gap Histogram
#----------------------------------------------------------

h <- df %>% filter(gap <= 15) %>%
  ggplot(aes(x = gap, fill=decade)) +
  geom_histogram(alpha = 0.6,
                 position = 'identity',
                 binwidth = 1) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Gap between surveys") +
  ylab("No. of surveys") +
  facet_grid(~decade)



