context("KMeans")	


test_that("KMeans trains as expected on synthetical dataset", {
	# generate synthetic data set (see stack overflow question), data is under tests/data/train.data
	
	set.seed(101)
	k <- 8
	x <- matrix(rnorm(25000, mean=0.5, sd=1), ncol=5)
	print(sum(x))
	E = yakmoR::orthoKMeansTrain (x = x, k = 8, rounds = 4, verbose = TRUE)

	dump("E", "A")
	
	centers1 = c(-0.773127221855639, 0.659700086388155, 0.329083376361123, 
    1.49625118777897, 0.873625000244238, -0.117523595254004, 
    1.11701279111456, 0.691882139142175, 0.523512940088078, 0.873967587176912, 
    -0.715550325210234, 0.961049187355381, 0.0590962848584614, 
    0.974772887297086, 0.102283762787814, 1.40187560175312, 0.514551753981911, 
    0.620528362142422, 0.0693927741188999, -0.365472127398107, 
    -0.450799384905508, 1.02898582722943, 1.68835444089093, 0.589286018013158, 
    0.195040796864759, 1.67387081498579, 0.44803991200936, 0.941416385472268, 
    0.0169186260829311, 1.14415353001268, 0.280135613335892, 
    -0.627756581985967, 1.05873417708419, 1.35937738140248, -0.293514189008916, 
    -0.00415706495722405, 1.43761247472097, -0.524762797420651, 
    0.758238978423363, 0.154921316330257)
    
	expect_equal (array(E$centers[[1]]), array(centers1))

	
    centers4 = c(0.010701278001286, 0.112359728297833, 
    0.169630944561386, -0.18544593058738, 0.0918849504837929, 
    -0.140045872509367, -0.214852304859258, 0.195689769253697, 
    0.193282175821165, -0.2578256689981, -0.540741284618697, 
    -0.102312748110501, 0.0931537603859549, 0.335845642799449, 
    -0.145221439518381, 0.178617658846293, -0.0792771102483505, 
    -0.169556427618685, 0.12413327716309, -0.123189777051957, 
    0.187673700549519, -0.13262631269392, 0.50666839383626, -0.253782200065002, 
    -0.0561105091134166, -0.275086893246053, 0.219192859456995, 
    0.0246430638467698, 0.0689115474152562, -0.287387602792217, 
    -0.156300069195894, 0.38918116739579, -0.486213057321351, 
    0.222106624424113, -0.0640555015659965, -0.0382777384379988, 
    0.0576820147200752, 0.13807130597526, 0.157853958039747, 
    0.145352333948816)
	
	expect_equal (array(E$centers[[4]]), array(centers4))
	
	stop()
    
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
	
	dump("E", "B")

	
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


