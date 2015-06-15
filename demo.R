#!/usr/bin/Rscript

set.seed(9)

library(devtools)
load_all(".")

	set.seed(101)
	k <- 8
	x <- matrix(rnorm(25000, mean=0.5, sd=1), ncol=5)
	E = yakmoR::orthoKMeansTrain (x = x, k = k, rounds = 4, verbose = TRUE)

	rounds = TRUE
	verbose = TRUE

	r = .Call('yakmoR_orthoKMeansTrainCpp', PACKAGE = 'yakmoR', 
		x = x, 
		k = k, 
		initType = init.type,
		rounds = rounds, 
		verbose = verbose)


library(testthat)
test()
