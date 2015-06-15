context("KMeans")	


test_that("KMeans works as expected on synthetical dataset", {
	# generate synthetic data set (see stack overflow question)
	
	set.seed(101)
	k <- 8
	x <- matrix(rnorm(25000, mean=0.5, sd=1), ncol=5)
	E = yakmoR::orthoKMeansTrain (x = x, k = 8, rounds = 4, verbose = TRUE)
		
		print (E)

	# just a quick check on a few entires
	expect_equal (E$centers[[4]][1,5], 0.0106450917141308)
 	expect_equal (E$centers[[4]][3,3], 0.00312835331550403 )
 	expect_equal (E$centers[[4]][8,1], 0.0218629625263223)

	# to get the output save above X as sparse data and run
	# $ ./yakmo ~/syn.data MODELA - -k 8 -m 4 -O 2 
	# $ ./yakmo - MODELA ~/syn.data -k 8 -m 4 -O 2 
	# note that both results DIFFER. i'd  have expected that
	# the output of both lines are the same, but it is not.
	# might be a bug. for now we work with this.
	
	# m = 1
	clusterhead = c(0, 1, 2, 3, 4, 5, 6, 5, 0, 7, 3, 1, 3, 4, 4, 0, 7, 5, 4)
	clustertail = c(1, 3, 3, 2, 1,  1, 3, 1, 0, 5,  4, 4, 2, 7, 3,  0, 5, 1, 2)

 	# m = 4
	clusterhead = c(0, 1, 2, 7, 4, 5, 0, 7, 5, 4, 4, 1, 4,  2, 4, 4, 3, 4, 2)
	clustertail = c(0, 4, 6, 7, 5, 7, 4, 1, 6, 2, 3, 0, 6, 6, 3, 4, 3, 2, 4)

	
 	expect_equal (E$cluster[[4]][1:19], clusterhead )
 	expect_equal (E$cluster[[4]][4982:5000], clustertail)

	# check the returned objective values
	expect_equal (E$obj[1], 13461.88738081)


	# test that it works with allmodels 
	E = yakmoR::orthoKMeansTrain(x, k = k, rounds = 2, verbose = TRUE)
	
	# just a quick check on a few entires
	expect_equal (E$centers[[1]][1,5], 1.2148486985733)
	expect_equal (E$centers[[1]][8,1], 0.303120678519)
	expect_equal (E$centers[[2]][1,1], -0.591699406545)
	expect_equal (E$centers[[2]][8,5], 0.57399077466425)

	
	# just a quick check on a few entires
	clusterhead = c(0, 1, 2, 3, 4, 5, 6, 5, 0, 7, 3, 1, 3, 4, 4, 0, 7, 5, 4)
	clustertail = c(1, 3, 3, 2, 1, 1, 3, 1, 0, 5, 4, 4, 2, 7, 3, 0, 5, 1, 2)
	expect_equal (E$cluster[[1]][1:19], clusterhead)
 	expect_equal (E$cluster[[1]][4982:5000], clustertail)

 	
	# just a quick check on a few entires
	clusterhead = c(0, 1, 4, 0, 1, 5, 4, 6, 7, 3, 4, 2, 5, 6, 4, 2, 0, 5, 5)
	clustertail = c(6, 0, 6, 5, 1, 7, 5, 1, 1, 7, 4, 6, 2, 5, 1, 4, 3, 2, 3)
 	expect_equal (E$cluster[[2]][1:19], clusterhead)
 	expect_equal (E$cluster[[2]][4982:5000], clustertail)

 	
	# check the returned objective values
	expect_equal (E$obj[1], 13461.88738081)
	expect_equal (E$obj[2], 5990.19781146)

	
	})

