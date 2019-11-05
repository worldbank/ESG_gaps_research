
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
library("lubridate")
library("RColorBrewer")
library("maps")
library("countrycode")

#----------------------------------------------------------
#   functions
#----------------------------------------------------------

maxN <- function(x, N=2, decreasing = FALSE){
  len <- length(x)
  if( N > len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  if (decreasing == FALSE) {
    sort(x,partial = len - N+1)[len - N+1]
  } else {
    -sort(-x,partial = len - N+1)[len - N+1]
  }
}


#----------------------------------------------------------
#   Download and Prepare Data
#----------------------------------------------------------

vars <- c("countrycode", "regioncode", "year", "datayear", "datatype")

# raw_df <-  povcalnet()
# save(raw_df, file = "data/povcalnet.RData")

load(file = "data/povcalnet.RData")

# temporal df
dft <- raw_df %>%
  group_by(countrycode, datatype, year) %>%
  mutate(n = n_distinct(coveragetype)) %>%
  filter((n > 1 & coveragetype %in% c("N", "A")) | n == 1) %>%
  group_by(countrycode)

# data type of max year
mtype <-  dft %>%
  filter(year == max(year)) %>%
  select(countrycode, mtype = datatype)

# join dft with datatype of max year
df <- dft %>%
  inner_join(mtype) %>%
  filter(datatype == mtype) %>% # keep datatype of max year in whole series
  mutate(n = n_distinct(datatype)) %>%
  filter(n == 1 | (n > 1 & datatype == "consumption")) %>% # if 2 datatypes in max year, keep con
  ungroup() %>% select(vars)

rm(dft, mtype)

df1 <- df %>%
  arrange(countrycode, datatype, year) %>%
  group_by(countrycode, datatype) %>%
  summarise(year1  = max(year),
            year2 = maxN(year)) %>%
  mutate(gap = year1 - year2, # difference betwen years
         decade = case_when(     # decade
           year1  %in% 1970:1979 ~ "1970s",
           year1  %in% 1980:1989 ~ "1980s",
           year1  %in% 1990:1999 ~ "1990s",
           year1  %in% 2000:2009 ~ "2000s",
           year1  %in% 2010:2020 ~ "2010s",
         ),
         yrs = year(ymd(year1, truncated = 2L)),
         lustrum = case_when(     # lustrum
           year1  %in% 1971:1979 ~ "1971-75",
           year1  %in% 1976:1980 ~ "1976-80",
           year1  %in% 1981:1985 ~ "1981-85",
           year1  %in% 1986:1990 ~ "1986-90",
           year1  %in% 1991:1995 ~ "1991-95",
           year1  %in% 1996:2000 ~ "1996-00",
           year1  %in% 2001:2005 ~ "2001-05",
           year1  %in% 2006:2010 ~ "2006-10",
           year1  %in% 2011:2015 ~ "2011-15",
           year1  %in% 2016:2020 ~ "2016-",
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
  mutate(country_count = n_distinct(countrycode)) %>%  # No. of countries per decade
  ungroup() %>%
  left_join(ci_name, by = c("countrycode" = "iso3c"))


df2 <- df %>%
  arrange(countrycode, datatype, year) %>%
  group_by(countrycode, datatype)  %>%
  mutate(gap = year - lag(year),
         decade = case_when(     # decade
           year  %in% 1970:1979 ~ "1970s",
           year  %in% 1980:1989 ~ "1980s",
           year  %in% 1990:1999 ~ "1990s",
           year  %in% 2000:2009 ~ "2000s",
           year  %in% 2010:2020 ~ "2010s",
         ),
         yrs = year(ymd(year, truncated = 2L)),
         gap_jump = case_when(     # GAP jumps
           gap  %in% 1:2   ~ "01-02",
           gap  %in% 3:5   ~ "03-05",
           gap  %in% 6:10  ~ "06-10",
           gap  %in% 11:20 ~ "11-20",
           gap  %in% 21:50 ~ "21-50",
         )) %>%
  filter(gap > 0) %>%   # remove repeated years with income and consumption
  group_by(decade) %>%
  mutate(country_count = n_distinct(countrycode)) %>%  # No. of countries per decade
  ungroup()



#----------------------------------------------------------
#   world map
#----------------------------------------------------------

world <- map_data("world")
cnames <- unique(world$region)
cnames <- tibble(region = cnames,
                 countrycode = countrycode(cnames, 'country.name', 'iso3c'))

world <- inner_join(world, cnames)
wdf <- inner_join(df1, world, by = c("countrycode" = "countrycode")) %>%
  mutate(text = paste0("Country: ", country, "\n",
                       "Gap: ", gap, " years\n")) %>%
  filter(!is.na(country))


plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5),
  legend.position = "bottom",
  legend.box = "horizontal"
)

brk <- c(1, 2, 3, 4, 5, 10, max(wdf$gap))
hh_map <- ggplot(data = wdf,
                 mapping = aes(x = long,
                               y = lat,
                               group = group)) +
  geom_polygon(aes(fill = gap)) +
  scale_fill_paletteer_c(package = "viridis",
                         palette = "plasma",
                         direction = 1,
                         breaks = brk) + # or direction=1
  ggtitle("Number of years between last two surveys") +
  borders("world", colour = "grey10") +
  coord_fixed(1.3) +
  labs(fill = "Years of gap") +
  guides(fill = guide_legend(title.position = "top")) +
  plain

phh_map <- ggplotly(hh_map, tooltip = "text")

#----------------------------------------------------------
#   Production of surveys over time
#----------------------------------------------------------

df3 <- df2 %>%
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
#   Calculations
#----------------------------------------------------------

nd <- df1 %>%
  group_by(region) %>%
  right_join(ci_name) %>%
  filter(is.na(gap)) %>%
  count() %>% ungroup()

nd_af <- nd %>% filter(str_detect(region, "Africa")) %>% summarise(sum(n))
nd_sa <- nd %>% filter(str_detect(region, "South")) %>% summarise(sum(n))
nd_ea <- nd %>% filter(str_detect(region, "Pacific")) %>% summarise(sum(n))
nd_la <- nd %>% filter(str_detect(region, "Latin")) %>% summarise(sum(n))


mg_af <- df1 %>% filter(str_detect(region, "Africa")) %>% summarise(mean(gap))
mg_sa <- df1 %>% filter(str_detect(region, "South")) %>% summarise(mean(gap))
mg_ea <- df1 %>% filter(str_detect(region, "Pacific")) %>% summarise(mean(gap))

