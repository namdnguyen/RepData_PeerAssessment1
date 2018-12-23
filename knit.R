#! /usr/bin/env Rscript
#
# Helper script to knit R-Markdown from command line from command line.

library(rmarkdown)
library(prettydoc)
library(knitr)

render("PA1_template.Rmd")
