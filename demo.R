#!/usr/bin/Rscript

set.seed(9)

library(devtools)
load_all(".")



library(testthat)
test()
