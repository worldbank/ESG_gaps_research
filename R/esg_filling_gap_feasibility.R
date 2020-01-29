# ==================================================
# project:       Feasibility of extrapolation based on volatiliy and coverage
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-01-28
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             chart
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------


#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------

# coefficient of variation
cv <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}

#----------------------------------------------------------
# Parameters
#----------------------------------------------------------


# threshold of coverage
# Threshold of coefficient of variations
# target year
# Max year available option2: as.numeric(format(Sys.Date(), "%Y")) - 1
# base year
fn_lstrank <- function(x,
                       th_cg = .5       ,
                       th_cv = .3       ,
                       tg_year = 2018   ,
                       mx_year = 2018   ,
                       bs_year = 2000)   {
  #----------------------------------------------------------
  #   Build data
  #----------------------------------------------------------

  # find max year per indicator/country
  maxy_ic <-
    x %>%  # Max year at the country and Indicator level
    group_by(indicatorID, indicator,  iso3c) %>%
    summarise(mxy = max(date, na.rm = TRUE))   # max year avaiable

  # last year density
  ialy <- maxy_ic %>%
    mutate(lst_y = if_else(mxy  %in% c(tg_year:mx_year), 1, 0)) %>% # is last year aviable?
    group_by(indicatorID, indicator) %>%
    summarise(lst_yd = mean(lst_y, na.rm = TRUE)) %>%               # last year density
    arrange(-lst_yd) %>%                                            # merge sector ID
    left_join(select(mtd, cetsid, sector),
              by = c("indicatorID" = "cetsid"))



  # Coverage threshold after base year
  yr_rg <- tg_year - bs_year # years range

  icgbs <-
    x %>%  # Indicador above coeverage threshold after base year
    filter(date >= bs_year &
             date < tg_year) %>%  # years available between base and target years
    group_by(indicatorID, iso3c) %>%
    summarise(nyrs = n()) %>%                     #  number of years
    mutate(coverage = nyrs / yr_rg) %>%             # coverage of years
    filter(coverage >= th_cg)     %>%             # keep coverage above threshold
    select(indicatorID, iso3c)

  # coefficient of variation

  cvic <-
    x %>%  # coefficient of variation by indicator and country
    filter(date >= bs_year &
             date < tg_year) %>%  # years available between base and target years
    inner_join(icgbs) %>%                         # keep obs according to coverage threshold
    group_by(indicatorID, indicator,  iso3c) %>%
    summarise(cv = cv(value)) %>%                 # coefficient of variation
    filter(cv <= th_cv)                           # filter those with low volatility


  # New max year and indicators and country level
  maxy_ic2 <- maxy_ic
  cond1 <-
    parse(text = "(maxy_ic2$indicatorID   %in% cvic$indicatorID &
                        maxy_ic2$iso3c  %in% cvic$iso3c)")

  maxy_ic2[eval(cond1) , "mxy"] <-  tg_year
  # last year density
  ialy2 <- maxy_ic2 %>%
    mutate(lst_y = if_else(mxy  %in% c(tg_year:mx_year), 1, 0)) %>% # is last year aviable?
    group_by(indicatorID, indicator) %>%
    summarise(lst_yd = mean(lst_y, na.rm = TRUE)) %>%               # last year density
    arrange(-lst_yd) %>%                                            # merge sector ID
    left_join(select(mtd, cetsid, sector),
              by = c("indicatorID" = "cetsid"))

  y <- list(ialy = ialy,
            ialy2 = ialy2)
  return(y)

}

chbar <- function(df) {
  ggplot(data = df) +
    geom_bar(stat = "identity",
             aes(
               x = reorder(indicatorID, lst_yd),
               y = lst_yd,
               fill = sector
             ),
             width = .75) +
    coord_flip() +
    #theme_clean() +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank())

}
