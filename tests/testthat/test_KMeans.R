context("KMeans")

test_that("KMeans works as expected on synthetical dataset", {
	# generate synthetic data set (see stack overflow question)
	
	set.seed(101)
	k <- 8
	X <- matrix(rnorm(25000, mean=0.5, sd=1), ncol=5)
	E = yakmoR::KMeans(X, k = k, verbose = TRUE)
	#print (E)
	expect_equal (1, 0)
})

