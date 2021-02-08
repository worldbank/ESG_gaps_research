# About This Project #

This repository contains research into the characteristics of statistical
indicators used for ESG sovereign wealth analysis. It is an RMarkdown project
that uses the [bookdown][bookdown] project to produce a paper
available in either HTML or PDF formats. Most of the analytical code is
written in R, but some functions are written in Python and integrated with the
[reticulate][reticulate] package. The outputs are available at
<https://worldbank.github.io/ESG_gaps_research>

# Requirements #

We built with:

* r version 3.6.2
* RStudio version 1.3
* Python 3.7.4
* bookdown 0.21
* wbstats 1.0.1

# Building the Project #

## PDF ##

In RStudio, choose "bookdown::tufte_book2" from the "Build Book" menu in the book tab

## HTML ##

In RStudio, choose "bookdown::tufte_html_book" from the "Build Book" menu in the book tab

There is an [apparent bug][mathjax-bug] in the HTML system that is preventing MathJax
equations (used in Appendix 2) from rendering correctly, and requires a post-build
fix. The apparent culprit is `plotly-latest.min.js` so the easiest fix is to remove
this reference since it isn't needed in the appendix.
In RStudio, you can run this code from the console after the build:

````
source("R/report.R")
mathjax_fix()
````

Then commit changes produced by the build and push them to github.

# Comments on Design #

One significant challenge of this project was designing visual content that worked in
both PDF and HTML outputs. Several additions were necessary on the PDF/LaTeX side
(see `mystyles.sty`). Interactive tables and graphs in HTML needed to be converted
to static outputs in PDF. This was accomplished through a set of wrapper functions
that provided reasonable defaults for each output while allowing output-specific
customization. These functions may all be found in `R/report.R` and are designed to
be resuable in similar projects with perhaps some changes to the default options.

[bookdown]:   https://bookdown.org
[reticulate]: https://cran.r-project.org/web/packages/reticulate/index.html
[mathjax-bug]: https://github.com/worldbank/ESG_gaps_research/issues/34
