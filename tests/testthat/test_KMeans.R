context("KMeans")	


test_that("KMeans trains as expected on synthetical dataset", {
	# generate synthetic data set (see stack overflow question), data is under tests/data/train.data
	
	set.seed(101)
	k <- 8
	x <- matrix(rnorm(25000, mean=0.5, sd=1), ncol=5)
	E = yakmoR::orthoKMeansTrain (x = x, k = 8, rounds = 4, verbose = TRUE)
		
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


test_that("KMeans predicts as expected on synthetical dataset", {
	# generate synthetic data set (see stack overflow question)

	# train first
	set.seed(101)
	k <- 8
	x <- matrix(rnorm(25000, mean=0.5, sd=1), ncol=5)
	obj = yakmoR::orthoKMeansTrain (x = x, k = 8, rounds = 4, verbose = TRUE)

	
	# check predictions for all rounds are done, data is under test/data/predict.data
	# get true output by 
	set.seed(100)
	x <- matrix(rnorm(25000, mean=0.5, sd=1), ncol=5)
	E = yakmoR::orthoKMeansPredict (x = x, obj = obj, verbose = TRUE)
	
	print( head(E$cluster))
	stop()
	# just a quick check on a few entires
	clusterhead1 = c(5, 2, 1, 3, 7, 3, 0, 3, 2, 5, 3, 3, 5, 5, 0, 3, 0, 6, 5)
	clustertail1 = c(6, 3, 6, 4, 2, 6, 3, 0, 2, 2, 4, 4, 3, 6, 5, 2, 6, 1, 3)	
	expect_equal (E$cluster[1:19,1], clusterhead1)
 	expect_equal (E$cluster[4982:5000,1], clustertail1)

	clusterhead2 = c(2, 7, 7, 1, 5, 6, 3, 2, 2, 2, 2, 6, 1, 1, 6, 0, 3, 2, 5)
	clustertail2 = c(0, 6, 4, 6, 5, 5, 3, 0, 3, 2, 4, 2, 1, 1, 2, 3, 1, 5, 3)
	expect_equal (E$cluster[1:19,2], clusterhead2)
 	expect_equal (E$cluster[4982:5000,2], clustertail2)

	clusterhead3 = c(4, 0, 1, 1, 4, 5, 5, 0, 0, 7, 7, 5, 1, 0, 0, 7, 7, 7, 2)
	clustertail3 = c(5, 5, 1, 4, 4, 1, 1, 1, 1, 1, 5, 7, 6, 3, 2, 5, 7, 0, 1)
	expect_equal (E$cluster[1:19,3], clusterhead3)
 	expect_equal (E$cluster[4982:5000,3], clustertail3)
	
	clusterhead4 = c(6, 0, 4, 4, 1, 1, 1, 4, 1, 4, 6, 7, 6, 3, 4, 6, 0, 1, 0)
	clustertail4 = c(7, 7, 4, 3, 3, 4, 5, 5, 4, 5, 5, 4, 2, 3, 3, 4, 4, 2, 5)
	expect_equal (E$cluster[1:19,4], clusterhead4)
 	expect_equal (E$cluster[4982:5000,4], clustertail4)
	
# tail
# 
# 6 0 5 7
# 3 6 5 7
# 6 4 1 4
# 4 6 4 3
# 2 5 4 3
# 6 5 1 4
# 3 3 1 5
# 0 0 1 5
# 2 3 1 4
# 2 2 1 5
# 4 4 5 5
# 4 2 7 4
# 3 1 6 2
# 6 1 3 3
# 5 2 2 3
# 2 3 5 4
# 6 1 7 4
# 1 5 0 2
# 3 3 1 5


# 	
# 5 2 4 6
# 2 7 0 0
# 1 7 1 4
# 3 1 1 4
# 7 5 4 1
# 3 6 5 1
# 0 3 5 1
# 3 2 0 4
# 2 2 0 1
# 5 2 7 4
# 3 2 7 6
# 3 6 5 7
# 5 1 1 6
# 5 1 0 3
# 0 6 0 4
# 3 0 7 6
# 0 3 7 0
# 6 2 7 1
# 5 5 2 0
	

	
})


