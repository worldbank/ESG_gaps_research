library(tidyverse)
library(tidytext)
library(reshape2)
library(ggiraph)
library(ggtext)
library(patchwork)
library(reticulate)
library(viridis)
# py_discover_config()
# py_config()
source("R/utils.R")
source_python("python/esg_loader.py")


# Load data ---------------------------------------------------------------

esg <- load_metadata(metafile_path = "./data/esg_metadata.csv",
                     datafile_path = "./data/ESG_wdi.feather")

# New column that divides sources into WBG and all other external
esg$domain <- if_else(esg$source_type != 'WBG', 'EXTERNAL', esg$source_type)

# Create some vectors of key display and logic fields
expl_a_g <- paste0('expl_', letters[1:7])
expl_c_g <- paste0('expl_', letters[3:7])
explans <- c('no_gap', expl_a_g)
core_fields <- c('sector','cetsid','input_name', explans)

# Prep data for plotting --------------------------------------------------


esg_copy <- esg %>%
  pivot_longer(cols = tidyselect::all_of(explans), names_to = "explanations") %>%
  mutate(
    explanations = recode(explanations,
                          expl_a = "Archive",
                          expl_b = "Stale",
                          expl_c = "Structural Lag",
                          expl_d = "Curation Lag",
                          expl_e = "Licensing",
                          expl_f = "Survey",
                          expl_g = "High income\nSmall country"
    )
  )


esg_all <- esg_copy %>%
  select(domain, source_type, sector, explanations, value) %>%
  filter(explanations != "no_gap", value == 1) %>%
  mutate(
    explanations = fct_rev(fct_infreq(explanations))
  )

esg_wb <- esg_copy %>%
  select(wbgv1, domain, source_type, sector, explanations, value)%>%
  filter(wbgv1 == 1,
         explanations != "no_gap",
         value == 1) %>%
  mutate(
    explanations = fct_rev(fct_infreq(explanations))
  )


# What are the main factors explaining the presence of data gaps? -------

esg_all_tmp <- esg_all %>%
  group_by(explanations, domain) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    dataset = "All indicators",
    explanations = as.character(explanations)
  )

esg_wb_tmp <- esg_wb %>%
  group_by(explanations, domain) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    dataset = "WBG dataset",
    explanations = as.character(explanations)
  )

esg_tmp <- bind_rows(esg_all_tmp,
                     esg_wb_tmp) %>%
  mutate(
    explanations = factor(explanations),
    explanations = fct_relevel(explanations,
                               c("Structural Lag",
                                 "Licensing",
                                 "Stale",
                                 "High income\nSmall country",
                                 "Archive",
                                 "Survey",
                                 "Method Lag"))
  )


smry_expl <- ggplot(esg_tmp) +
  # geom_col_interactive(aes(x = explanations, y = n,
  #                          text = paste("Data producer:", domain,
  #                                          "<br />Number of gaps:", n),
  #                          fill = domain),
  #                      alpha = .8) +
  geom_col(aes(x = explanations, y = n,
               text = paste("Data producer:", domain,
                            "<br />Number of gaps:", n),
               fill = domain),
           alpha = .8) +
  #scale_fill_viridis_d(option = "E", alpha = .8) +
  scale_fill_manual(values = my_palette2) +
  scale_x_reordered() +
  coord_flip() +
  theme_esg() +
  facet_wrap(~dataset,
             nrow = 1)


# Main explanations for data gaps by sector---------------------------
esg_all_tmp <- esg_all %>%
  group_by(explanations, sector) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    dataset = "All indicators",
    explanations = as.character(explanations)
  )

esg_wb_tmp <- esg_wb %>%
  group_by(explanations, sector) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    dataset = "WBG dataset",
    explanations = as.character(explanations)
  )

esg_tmp <- bind_rows(esg_all_tmp,
                     esg_wb_tmp) %>%
  mutate(
    explanations = factor(explanations),
    explanations = fct_relevel(explanations,
                               c("Curation Lag",
                                 "Licensing",
                                 "Stale",
                                 "High income\nSmall country",
                                 "Archive",
                                 "Survey",
                                 "Structural Lag"))
  )


smry_expl_sctr <- ggplot(esg_tmp) +
  geom_col(aes(x = explanations, y = n,
               text = paste("Sector:", sector,
                            "<br />Number of gaps:", n),
               fill = sector),
           alpha = .8) +
  scale_fill_manual(values = my_palette3) +
  scale_x_reordered() +
  coord_flip() +
  theme_esg() +
  facet_grid(.~dataset,
             scales = "free_y")
# Sector view
esg_tmp <- esg_tmp %>%
  mutate(
    sector = fct_rev(fct_relevel(sector,
                                     c("SOC",
                                       "ENV",
                                       "GOV"))
                     ),
    explanations = fct_relevel(explanations,
                               c("Curation Lag",
                                 "Licensing",
                                 "Stale",
                                 "High income\nSmall country",
                                 "Archive",
                                 "Survey",
                                 "Structural Lag"))
  )

smry_expl_sctr2 <- ggplot(esg_tmp) +
  geom_col(aes(x = sector,
               y = n,
               fill = explanations,
               text = paste("Sector:", explanations,
                            "<br />Number of gaps:", n)),
           alpha = .8) +
  scale_fill_viridis_d(option = "A", alpha = .8) +
  coord_flip() +
  theme_esg() +
  facet_wrap(.~dataset,
             scales = "free_y") #+
  # theme(
  #   legend.position = "bottom"
  # )





# Share of externally sourced indicators ----------------------------------

esg_all_tmp <- esg_all %>%
  group_by(explanations) %>%
  mutate(
    total = sum(value)
  ) %>%
  group_by(domain, explanations) %>%
  mutate(
    total_exp = sum(value)
  ) %>%
  select(domain, explanations, total_exp, total) %>%
  distinct() %>%
  summarise(
    percent_ext = total_exp / total
  ) %>%
  filter(domain == "EXTERNAL")

tmp <- esg_all %>%
  group_by(domain) %>%
  summarise(
    total = sum(value)
  )
average <- tmp$total[tmp$domain == "EXTERNAL"] / sum(tmp$total)


shr_ext <- ggplot() +
  geom_col(data = esg_all_tmp,
           aes(x = reorder(explanations, percent_ext),
               y = percent_ext,
               text = paste("Explanation:", explanations,
                            "<br />Externally sourced:", scales::percent(percent_ext))),
           fill = "#721F81FF",
           alpha = .8
  ) +
  geom_hline(yintercept = average, color = "#F1605DFF", linetype = "longdash",
             size = rel(1.5)) +
  annotate("text",
           x = 2,
           y = .9,
           label = paste0("Average of\nexternally-sourced\nindicators\n(", scales::percent(average), ")"),
           color = "#F1605DFF") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_esg()

# Share of externally sourced indicators by sector ----------------------------------

esg_all_tmp <- esg_all %>%
  group_by(explanations, sector) %>%
  mutate(
    total = sum(value)
  ) %>%
  group_by(domain, explanations, sector) %>%
  mutate(
    total_exp = sum(value)
  ) %>%
  select(domain, explanations, sector, total_exp, total) %>%
  distinct() %>%
  summarise(
    percent_ext = total_exp / total
  ) %>%
  filter(domain == "EXTERNAL")

tmp <- esg_all %>%
  group_by(domain, sector) %>%
  summarise(
    total = sum(value)
  ) %>%
  group_by(sector) %>%
  mutate(
    gd_total = sum(total)
  ) %>%
  ungroup() %>%
  filter(domain == "EXTERNAL") %>%
  mutate(
    average = total / gd_total
  ) %>%
  select(sector, average)


shr_ext_sctr <- ggplot() +
  geom_col(data = esg_all_tmp,
           aes(x = reorder(explanations, percent_ext),
               y = percent_ext,
               text = paste("Explanation:", explanations,
                            "<br />Externally sourced:", scales::percent(percent_ext))),
           fill = "#721F81FF",
           alpha = .8
  ) +
  geom_hline(data = tmp,
             aes(yintercept = average), color = "#F1605DFF", linetype = "longdash",
             size = rel(1.5)) +
  # annotate("text",
  #          x = 2,
  #          y = .9,
  #          label = paste0("Average of\nexternally-sourced\nindicators\n(", scales::percent(average), ")"),
  #          color = "#F1605DFF") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_esg() +
  facet_grid(.~sector)
