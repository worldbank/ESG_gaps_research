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
  mutate(wb = ifelse(grepl("[Ww]orld [Bb]ank", source1_name),
                     "World Bank",
                     "Other Institution"))

mtd$wbgv1 <- factor(mtd$wbgv1,
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
    t <- tabular((Source = wbgv1 + 1)  ~ Format(digits=0)*(all=1) +
                   Heading()*(get(expl))*
                   ((Sector = sector)*
                      (Format(digits=0)*(n=1) +
                         Format(digits=1)*(Share=mean))
                   ),
                 data = mtd)
  } else if (tbl == 2) {

    # Share and number of indicators (LONG)
    t <- tabular((Sector = sector)*(Source = wbgv1)  ~
                   Heading()*(get(expl))*((Format(digits=0)*(n=1) +
                                             Format(digits=1)*(Share=mean))),
                 data = mtd)
  } else {
    # Just the share
    t <- tabular((Source = wbgv1)  ~ Format(digits=2)*Heading()*
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

