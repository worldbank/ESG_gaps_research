
# preview just one chapter
file <- c("index.Rmd")
bookdown::render_book(file, "bookdown::gitbook",
                      preview = TRUE)
beepr::beep(10)

# render the whole book
bookdown::render_book("index.Rmd", "bookdown::gitbook")
beepr::beep(10)

#serve the whole book to see modification on the fly
dir <- getwd()
bookdown::serve_book(dir = dir, output_dir = "docs",
                     preview = TRUE)
