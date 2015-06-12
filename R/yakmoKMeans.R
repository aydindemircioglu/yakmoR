

#'
#' 
#' 
#' 
yakmoKMeans. <- function(x, centers, iter.max = 100L, rounds = 1L, verbose = FALSE, allmodels = FALSE) {

	# checkmate checks

    r = .Call('yakmoR_KMeans', PACKAGE = 'yakmoR', 
		x = x, 
		k = k, 
		iter = iter.max, 
		m = rounds, 
		verbose = verbose, 
		allmodels = allmodels)

	obj = makeS3Object ()
	return (obj)
}


#'
#' 
#' 
#' 
yakmoKMeans.predict <- function (x, round =  1, verbose = FALSE) {
	
	# checkmate checks

	# if multiple orthogonal rounds have been trained,
	# we return a matrix of all predictions
	
	# call
	r = .Call('yakmoR_KMeans', PACKAGE = 'yakmoR', 
		x = x, 
		k = k, 
		iter = iter.max, 
		m = rounds, 
		verbose = verbose, 
		allmodels = allmodels)

}


KMeans = function (x, centers, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", 
    "Lloyd", "Forgy", "MacQueen"), trace = FALSE) 
{
	# make sure x has the right structure
	x <- as.matrix(x)
	m <- as.integer(nrow(x))
	if (is.na(m)) 
		stop("invalid nrow(x)")
	p <- as.integer(ncol(x))
	if (is.na(p)) 
		stop("invalid ncol(x)")
	if (missing(centers)) 
		stop("'centers' must be a number or a matrix")

	# ?
    storage.mode(x) <- "double"
    
    # take care of centers
    if (length(centers) == 1L) {
        k <- centers
        if (nstart == 1) 
            centers <- x[sample.int(m, k), , drop = FALSE]
        if (nstart >= 2 || any(duplicated(centers))) {
            cn <- unique(x)
            mm <- nrow(cn)
            if (mm < k) 
                stop("more cluster centers than distinct data points.")
            centers <- cn[sample.int(mm, k), , drop = FALSE]
        }
    }
    else {
        centers <- as.matrix(centers)
        if (any(duplicated(centers))) 
            stop("initial centers are not distinct")
        cn <- NULL
        k <- nrow(centers)
        if (m < k) 
            stop("more cluster centers than data points")
    }
    k <- as.integer(k)
    if (is.na(k)) 
        stop("'invalid value of 'k'")
    iter.max <- as.integer(iter.max)
    if (is.na(iter.max) || iter.max < 1) 
        stop("'iter.max' must be positive")
    if (ncol(x) != ncol(centers)) 
        stop("must have same number of columns in 'x' and 'centers'")
    storage.mode(centers) <- "double"
    Z <- do_one(nmeth)
    best <- sum(Z$wss)
    if (nstart >= 2 && !is.null(cn)) 
        for (i in 2:nstart) {
            centers <- cn[sample.int(mm, k), , drop = FALSE]
            ZZ <- do_one(nmeth)
            if ((z <- sum(ZZ$wss)) < best) {
                Z <- ZZ
                best <- z
            }
        }
        

	# check for empty clusters
# if (m23 <- any(nmeth == c(2L, 3L))) {
#             if (any(Z$nc == 0)) 
#                 warning("empty cluster: try a better set of initial centers", 
#                   call. = FALSE)
#         }

	# check for convergence
#         if (Z$iter > iter.max) {
#             warning(sprintf(ngettext(iter.max, "did not converge in %d iteration", 
#                 "did not converge in %d iterations"), iter.max), 
#                 call. = FALSE, domain = NA)
#             if (m23) 
#                 Z$ifault <- 2L
#         }
        
        
        
    centers <- matrix(Z$centers, k)
    dimnames(centers) <- list(1L:k, dimnames(x)[[2L]])
    cluster <- Z$c1
    if (!is.null(rn <- rownames(x))) 
        names(cluster) <- rn
    totss <- sum(scale(x, scale = FALSE)^2)
    structure(list(cluster = cluster, centers = centers, totss = totss, 
        withinss = Z$wss, tot.withinss = best, betweenss = totss - 
            best, size = Z$nc, iter = Z$iter, ifault = Z$ifault), 
        class = "kmeans")
}
