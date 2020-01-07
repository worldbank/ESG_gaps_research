# library(ggmosaic)
# library(ggiraph)
# library(ggtext)
# library(extrafont)

plot_mosaic <- function(df,
                        explanation = "explanation A",
                        fill_colors = c("#cf455c", "#444444"),
                        alpha = .7) {
  # my_title <- paste0("Data gaps due to ", explanation)
  # my_subtitle <- paste0('<b style="color:#cf455c">Red areas</b> show the proportion of excluded indicators')

  p <- ggplot(data = df) +
    geom_mosaic(aes(x = product(sector), fill = factor(status), na.rm = TRUE), alpha = alpha) +
    labs(#title = my_title,
         #subtitle = my_subtitle,
         x = "",
         y = "") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = fill_colors) +
    ggthemes::theme_hc() +
    theme(
      axis.ticks.x = element_blank(),
      legend.position = "none",
      text = element_text(family = "Calibri")
    )

  labels <- ggplot_build(p)$data[[1]] %>%
    group_by(x1__sector) %>%
    mutate(
      percent = paste0(round(.wt / sum(.wt) * 100, 1), "%")
    ) %>%
    ungroup()

  p <- p + geom_text(data = labels,
                     aes(x = (xmin + xmax) / 2,
                         y = (ymin + ymax) / 2,
                         label = percent))

  return(p)
}
