
# preview just one chapter
file <- c("ESG_hhsAvailability.Rmd")
file <- c("ESG_availability.Rmd")
bookdown::render_book(file, "bookdown::tufte_html_book",
                      preview = TRUE)
beepr::beep(10)

# render the whole book
bookdown::render_book("index.Rmd", "bookdown::tufte_html_book")
beepr::beep(10)

#serve the whole book to see modification on the fly
dir <- getwd()
bookdown::serve_book(dir = dir, output_dir = "docs",
                     preview = TRUE)


