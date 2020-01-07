# ==================================================
# project:       Explanation of analysis
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    Oct 04 2019
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             charts and tables
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------


#----------------------------------------------------------
# cleaning data
#----------------------------------------------------------

mtd <- mtd %>%
  mutate(wb = ifelse(grepl("[Ww]orld [Bb]ank", source1_name), 1,0))

mtd$wb <- factor(mtd$wb,
                levels = c(0,1),
                labels = c("Other sources", "World Bank"))


mtd$sector <- as.factor(mtd$sector)

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
expl_tb <- function(expl, tbl = 1){
  expl <- quo_name(enquo(expl))

  if (tbl == 1) {
    # Share and number of indicators (WIDE)
    t <- tabular((Source = wb + 1)  ~ Format(digits=0)*(all=1) +
                   Heading()*(get(expl))*
                   ((Sector = sector)*
                      (Format(digits=0)*(n=1) +
                         Format(digits=1)*(Share=mean))
                   ),
                 data = mtd)

    t <- as.data.frame.matrix(t)

    rownames(t) <- c("Other Sources", "World Bank", "All")
    t <- rownames_to_column(t, "Source")
    t <- type.convert(t,  as.is = TRUE)

  } else if (tbl == 2) {

    # Share and number of indicators (LONG)
    t <- tabular((Sector = sector)*(Source = wb)  ~
                   Heading()*(get(expl))*((Format(digits=0)*(n=1) +
                                             Format(digits=1)*(Share=mean))),
                 data = mtd)
  } else {
    # Just the share
    t <- tabular((Source = wb)  ~ Format(digits=2)*Heading()*
                   (get(expl))*(Sector = sector)*Heading()*(mean)*
                   DropEmpty(empty="."),
                 data = mtd)
  }

  return(t)
}

#----------------------------------------------------------
#   create tables
#----------------------------------------------------------

expl_var <- names(mtd)[str_detect(names(mtd),"expl_")]
t <- lapply(expl_var, expl_tb)


#----------------------------------------------------------
#   Prep data for plotting
#----------------------------------------------------------

explanation_lkup <- c("explanation A",
                      "explanation B",
                      "explanation C",
                      "explanation D",
                      "explanation E",
                      "explanation F",
                      "explanation G",
                      "explanation H")
names(explanation_lkup) <- c("expl_a",
                             "expl_b",
                             "expl_c",
                             "expl_d",
                             "expl_e",
                             "expl_f",
                             "expl_g",
                             "expl_h")

mtd_long <- mtd %>%
  select(sector, origin = wb, expl_a:expl_h) %>%
  pivot_longer(cols = -c("sector", "origin"), names_to = "reason", values_to = "status") %>%
  mutate(
    sector = as.character(sector),
    sector = recode(sector, ENV = "Environment", GOV = "Governance", SOC = "Social"),
    status = recode(status, `0` = "Included", `1` = "Excluded"),
    origin = as.character(origin)
  ) %>%
  group_by(sector, reason) %>%
  mutate(
    n_indicators = n()
  ) %>%
  ungroup() %>%
  mutate(
    sector = paste0(sector, "\n(", n_indicators, " indicators)")
  )

mtd_long$reason <- explanation_lkup[mtd_long$reason]

plot_list <- purrr::map(explanation_lkup, function(x) {

  df <- mtd_long[mtd_long$reason == x, ]
  out <- plot_mosaic(df, explanation = x)
  return(out)
})
