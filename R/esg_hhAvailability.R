
#----------------------------------------------------------
#   library
#----------------------------------------------------------


#remotes::install_github("worldbank/povcalnetR")
library("povcalnetR")
library("tidyverse")
library("hrbrthemes")
library("plotly")
library("viridis")
library("zoo")
library("paletteer")


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
         yrs = year(ymd(year, truncated = 2L)),
         lustrum = case_when(     # lustrum
           year  %in% 1971:1979 ~ "1971-75",
           year  %in% 1976:1980 ~ "1976-80",
           year  %in% 1981:1985 ~ "1981-85",
           year  %in% 1986:1990 ~ "1986-90",
           year  %in% 1991:1995 ~ "1991-95",
           year  %in% 1996:2000 ~ "1996-00",
           year  %in% 2001:2005 ~ "2001-05",
           year  %in% 2006:2010 ~ "2006-10",
           year  %in% 2011:2015 ~ "2011-15",
           year  %in% 2016:2020 ~ "2016-",
         ),
         gap_jump = case_when(     # GAP jumps
           gap  %in% 1:2   ~ "01-02",
           gap  %in% 3:5   ~ "03-05",
           gap  %in% 6:10  ~ "06-10",
           gap  %in% 11:20 ~ "11-20",
           gap  %in% 21:50 ~ "21-50",
         )) %>%
  filter(gap > 0) %>%   # remove repeated years with income and consumption
  group_by(decade) %>%
  mutate(country_count = n_distinct(countrycode)) # No. of countries per decade

#----------------------------------------------------------
#   Heatmap of surveys per decada and production Gap
#----------------------------------------------------------

d <- quo(lustrum)

df_hh <- df %>%
  group_by(gap_jump, !! d) %>%
  summarise(n_country = n_distinct(countrycode)) %>%
  mutate(text = paste0("Period: ", !! d, "\n",
                       "Gap: ", gap_jump, " years\n",
                       "No. countries: ",n_country,"\n"))

brk <- waiver()
brk <- c(5, 10, 20, 40, 60, 70)
g_hh <- ggplot(df_hh, aes(x = !! d,
               y = gap_jump,
               fill = n_country,
               text = text)) +
  geom_tile() +
  scale_fill_paletteer_c(package = "viridis", palette = "viridis",
                         breaks = brk) +
  theme_ipsum() +
  ggtitle(label = "Number of countries per survey gap") +
  theme(legend.position = "bottom",
        legend.box = "horizontal") +
  labs(fill = "Number of countries",
       y = "Gap between surveys in years",
       x = "Period") +
  guides(fill = guide_legend(title.position = "top"))

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
    geom_area(color = "black" , aes(fill = regioncode),
              position = 'stack', size = .5, alpha = .9) +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top")) +
  labs(fill = "Region code",
       x = "Year",
       y = "Number of surveys")

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



