

#' orthogonal kmeans training function
#' 
#'  @param		x	data to cluster
#'  @param		k	number of centroids
#'  @param		rounds	number of rounds/views for orthogonal kmeans
#'  @param		iter.max	number of maximal iterations for each clustering
#'  @param		init.type	string with method to initialize centroids
#'  @param		verbose	show verbose messages?
#'
#'  @export
orthoKMeansTrain <- function(x = NULL, 
	k = NULL, 
	rounds = 1, 
	iter.max = 100, 
	init.type = "KMeans++", 
	verbose = FALSE) 
{
	# checkmate checks
	if (verbose == TRUE)
		message ("Checking arguments.")

	checkmate::assertMatrix(x, min.rows = k + 1)
	checkmate::assertCount(k)
	checkmate::assertCount(rounds)
	checkmate::assertCount(iter.max)
	checkmate::assertString (init.type)
	checkmate::assertFlag (verbose)
	
	if (init.type == "Random")
		initType = 0
	else if (init.type == "KMeans++")
		initType = 1
	else 
		stop ("Unknown centroid initialization method.")
	
	# call main function
	if (verbose == TRUE)
		message ("Calling C++ function.")

	random = FALSE
	
	r = .Call('yakmoR_orthoKMeansTrainCpp', PACKAGE = 'yakmoR', 
		x = x, 
		rounds = rounds, 
		k = k, 
		iter = iter.max, 
		initType = initType,
		random = random,
		verbose = verbose)

	# wrap list as object
	obj = BBmisc::makeS3Obj ("yakmoR",
		obj = r$obj,
		k = k, 
		iter = iter.max, 
		rounds = rounds, 
		centers = r$centers,
		cluster = r$cluster,
		nf = r$nf
	)

	return (obj)
}



#' orthogonal kmeans prediction function
#' 
#'  @param		x	data to assign clusters
#'  @param		rounds	list of views, if multiple views were computed
#'  @param		verbose	show verbose messages?
#'  @return		numericmatrix with as many colums as specified in the rounds array
#'
#'  @export
orthoKMeansPredict <- function (x, obj = NULL, verbose = FALSE) {

	# checkmate checks
	checkmate::assertClass (obj, "yakmoR")
	checkmate::assertMatrix(x, min.rows = 1)
	checkmate::assertFlag (verbose)

	# if multiple orthogonal rounds have been trained,
	# we return a matrix of all predictions

# 	# call
	r = .Call('yakmoR_orthoKMeansPredictCpp', PACKAGE = 'yakmoR', 
		x = x, 
		obj$centers,
		obj$nf,
		obj$k,
		verbose = verbose)
}


# 
# KMeans = function (x, centers, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", 
#     "Lloyd", "Forgy", "MacQueen"), trace = FALSE) 
# {
# 	# make sure x has the right structure
# 	x <- as.matrix(x)
# 	m <- as.integer(nrow(x))
# 	if (is.na(m)) 
# 		stop("invalid nrow(x)")
# 	p <- as.integer(ncol(x))
# 	if (is.na(p)) 
# 		stop("invalid ncol(x)")
# 	if (missing(centers)) 
# 		stop("'centers' must be a number or a matrix")
# 
# 	# ?
#     storage.mode(x) <- "double"
#     
#     # take care of centers
#     if (length(centers) == 1L) {
#         k <- centers
#         if (nstart == 1) 
#             centers <- x[sample.int(m, k), , drop = FALSE]
#         if (nstart >= 2 || any(duplicated(centers))) {
#             cn <- unique(x)
#             mm <- nrow(cn)
#             if (mm < k) 
#                 stop("more cluster centers than distinct data points.")
#             centers <- cn[sample.int(mm, k), , drop = FALSE]
#         }
#     }
#     else {
#         centers <- as.matrix(centers)
#         if (any(duplicated(centers))) 
#             stop("initial centers are not distinct")
#         cn <- NULL
#         k <- nrow(centers)
#         if (m < k) 
#             stop("more cluster centers than data points")
#     }
#     k <- as.integer(k)
#     if (is.na(k)) 
#         stop("'invalid value of 'k'")
#     iter.max <- as.integer(iter.max)
#     if (is.na(iter.max) || iter.max < 1) 
#         stop("'iter.max' must be positive")
#     if (ncol(x) != ncol(centers)) 
#         stop("must have same number of columns in 'x' and 'centers'")
#     storage.mode(centers) <- "double"
#     Z <- do_one(nmeth)
#     best <- sum(Z$wss)
#     if (nstart >= 2 && !is.null(cn)) 
#         for (i in 2:nstart) {
#             centers <- cn[sample.int(mm, k), , drop = FALSE]
#             ZZ <- do_one(nmeth)
#             if ((z <- sum(ZZ$wss)) < best) {
#                 Z <- ZZ
#                 best <- z
#             }
#         }
#         
# 
# 	# check for empty clusters
# # if (m23 <- any(nmeth == c(2L, 3L))) {
# #             if (any(Z$nc == 0)) 
# #                 warning("empty cluster: try a better set of initial centers", 
# #                   call. = FALSE)
# #         }
# 
# 	# check for convergence
# #         if (Z$iter > iter.max) {
# #             warning(sprintf(ngettext(iter.max, "did not converge in %d iteration", 
# #                 "did not converge in %d iterations"), iter.max), 
# #                 call. = FALSE, domain = NA)
# #             if (m23) 
# #                 Z$ifault <- 2L
# #         }
#         
#         
#         
#     centers <- matrix(Z$centers, k)
#     dimnames(centers) <- list(1L:k, dimnames(x)[[2L]])
#     cluster <- Z$c1
#     if (!is.null(rn <- rownames(x))) 
#         names(cluster) <- rn
#     totss <- sum(scale(x, scale = FALSE)^2)
#     structure(list(cluster = cluster, centers = centers, totss = totss, 
#         withinss = Z$wss, tot.withinss = best, betweenss = totss - 
#             best, size = Z$nc, iter = Z$iter, ifault = Z$ifault), 
#         class = "kmeans")
# }
