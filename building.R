
# preview just one chapter
file <- c("ESG_hhsAvailability.Rmd")

file <- c("ESG_volatility.Rmd")
bookdown::render_book(file, preview = TRUE)
bfile <- paste0("docs/", sub("(.*)(\\.Rmd)", "\\1", file), ".html")
browseURL(bfile)
beepr::beep(10)

# render the whole book
bookdown::render_book("index.Rmd")
browseURL("docs/index.html")
beepr::beep(10)

#serve the whole book to see modification on the fly
dir <- getwd()
bookdown::serve_book(dir = dir, output_dir = "docs",
                     preview = TRUE)


