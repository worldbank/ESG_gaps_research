
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

df2 <- df %>%
  group_by(gap, decade) %>%
  summarise(n_country = n_distinct(countrycode)) %>%
  mutate(text = paste0("Decade: ", decade, "\n",
                       "Gap: ", gap, "\n",
                       "No. countries: ",n_country,"\n"))


g <- ggplot(df2, aes(x = decade,
               y = gap,
               fill = n_country,
               text = text)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdPu") +
  theme_ipsum() +
  ggtitle(label = "No. of countries per survey gap")

ggplotly(g, tooltip = "text")


#----------------------------------------------------------
#   Production of surveys over time
#----------------------------------------------------------

df3 <- df %>%
  group_by(yrs) %>%
  summarise(n_country = n_distinct(countrycode)) %>%
  mutate(text = paste0("Year: ", yrs, "\n",
                  "No. countries: ",n_country,"\n"))


p <- ggplot(df3, aes(x = yrs, y = n_country, text = text, group = 1)) +
  geom_area(fill = "#69b3a2", alpha = 0.5) +
  geom_line(color = "#69b3a2") +
  ylab("Number of Surveys") +
  xlab("Year") +
  theme_ipsum()

ggplotly(p, tooltip = "text")



