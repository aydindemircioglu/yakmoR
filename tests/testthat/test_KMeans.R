context("KMeans")

test_that("KMeans works as expected on synthetical dataset", {
	# generate synthetic data set (see stack overflow question)
	
	set.seed(101)
	k <- 8
	X <- matrix(rnorm(25000, mean=0.5, sd=1), ncol=5)
	E = yakmoR::KMeans(X, k = k, m = 2, verbose = TRUE)

	# just a quick check on a few entires
	expect_equal (E$model$`0`[1,5], 1.2148486985733)
	expect_equal (E$model$`0`[8,1], 0.303120678519)
	expect_equal (E$model$`1`[1,1], -0.591699406545)
	expect_equal (E$model$`1`[8,5], 0.57399077466425)
	
	# check the returned objective values
	expect_equal (E$obj[1], 13461.88738081)
	expect_equal (E$obj[2], 5990.19781146)

})

