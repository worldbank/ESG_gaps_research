
mathjax_fix <- function(file='docs/extended-coverage-analysis.html') {
  # Automated fix for https://github.com/worldbank/ESG_gaps_research/issues/34
  if( require(stringr) ) {
    doc = readLines(file)
    writeLines(doc[!str_detect(doc, 'plotly-latest.min.js')], file)
  }
  else {
    print('stringr must be installed to use this function')
  }
}

report.graphic <- function(viz, name, html=list(), pdf=list()) {
  # generates an output-specific chart or interacive graphic
  # viz: a chart from ggplot
  # name: logical name of the chart; this is used to generate PNG graphics
  # html and pdf are named lists with options for each output mode.
  # Each with the following (optional) elements:
  #   args: a list of named parameters to ggplotly or ggsave
  #   layers: output-specific layers: added to the graph
  #   post:   a post-processing function. Passed the graph as an argument: return a graph (html only)

  if( knitr::is_html_output() ) {
    args = list(tooltip='text')
    if( !is.null(html[['args']]) ) args = modifyList(args, html[['args']])

    g = do.call(plotly::ggplotly, c(list(viz + html[['layers']]), args))
    if( !is.null(html[['post']]) ) return(html[['post']](g))

    return(g)
    # return(plotly::ggplotly(viz, tooltip='text'))
  }

  if( knitr::is_latex_output() ) {
    common_themes = list(
      theme(legend.title=element_text(size=rel(0.5))),
      theme(legend.text=element_text(size=rel(0.5)))
    )
    args = list(plot=viz + common_themes + pdf[['layers']], width=3, units="in", dpi="retina")
    if( !is.null(pdf[['args']]) ) args = modifyList(args, pdf[['args']])

    path <- paste('figs/', name, '.png', sep='')
    g = do.call(ggsave, c(list(path), args))
    # ggsave(path, plot = g1, width = 7, height = 7, dpi = "retina")
    return(knitr::include_graphics(path))
  }
}

report.table <- function(t, columns, caption, align=NULL, html=list(), pdf=list()) {

  if(knitr::is_html_output()) {
    # escape special characters that screw up HTML tables for some reason
    t_ = mutate_all(t, function(i) {gsub("\\$", "\\\\$", i)})
    kt = knitr::kable(t_, format = "html", col.names=columns, caption=caption, align=align,
                      row.names=FALSE, table.attr='class="striped"')
    widths = html[['widths']]
  }

  if (knitr::is_latex_output()) {
    args = list(latex_options="striped")
    if( !is.null(pdf[['args']]) ) args = modifyList(args, pdf[['args']])

    kt = knitr::kable(t, booktabs = TRUE, col.names=columns, caption=caption,
                      row.names=FALSE, align=align)
    kt = do.call(kableExtra::kable_styling, c(list(kt), args))
    widths = pdf[['widths']]
  }

  if( !is.null(widths)) {
    for( i in 1:length(widths) ) {
      if( !is.null(widths[[i]])) {
        kt = kt %>% column_spec(i, width=paste0(widths[[i]], "em"))
      }
    }
  }

  return(kt)
}

report.datatable <- function(t, columns, caption, precision=list(), html=list(), pdf=list()) {

  if(knitr::is_html_output()) {
    args = list(pagelength=5, autowidth=TRUE)
    if( !is.null(html[['args']]) ) args = modifyList(args, html[['args']])
    table = DT::datatable(t,
                          colnames = columns,
                          caption = caption,
                          rownames = FALSE,
                          filter = 'top',
                          width = '55%',
                          extensions = 'FixedColumns',
                          options = args)

    for(i in names(precision)) {
      if( str_length(i) > 0 ) {
        table = table %>% DT::formatRound(i, precision[[i]])
      }
    }

    return(table)
  }

  if(knitr::is_latex_output()) {
    args = list(args=list(font_size=6))
    args = modifyList(args, pdf)
    return(report.table(t, columns, caption, pdf=args))
  }
}
