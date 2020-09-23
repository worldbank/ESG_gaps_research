#
# # preview just one chapter
#
# file <- c("ESG_volatility.Rmd")
#
# file <- c("ESG_hhsAvailability.Rmd")
#
# file <- c("ESG_availability.Rmd")
# file <- c("ESG_resultsbyexplanation.Rmd")
file <- c("ESG_coverage.Rmd")
bookdown::render_book(file, preview = TRUE)
bookdown::render_book("index.Rmd", preview = TRUE)
bookdown::render_book("ESG_background.Rmd", preview = TRUE)
bookdown::render_book("ESG_coverage.Rmd", preview = TRUE)
bookdown::render_book("ESG_explanation.Rmd", preview = TRUE)
bookdown::render_book("ESG_variability.Rmd", preview = TRUE)
bookdown::render_book("ESG_discussion.Rmd", preview = TRUE)
bookdown::render_book("ESG_appendix1.Rmd", preview = TRUE)
bookdown::render_book("ESG_appendix2.Rmd", preview = TRUE)
beepr::beep(10)
# browseURL("docs/volatility.html")
browseURL("docs/coverage.html")
#
# browseURL("docs/background.html")


# bfile <- paste0("docs/", sub("(.*)(\\.Rmd)", "\\1", file), ".html")

# render the whole book
bookdown::render_book("index.Rmd")
beepr::beep(10)
browseURL("docs/index.html")

#serve the whole book to see modification on the fly
# dir <- getwd()
# bookdown::serve_book(dir = dir, output_dir = "docs",
#                      preview = TRUE)


#--------- render as PDF ---------
bookdown::render_book("index.Rmd", "bookdown::pdf_book")
