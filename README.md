a simple wrapper for the kmeans c++ library yakmo

yakmo implements orthogonal k-means (hence its name). 
in each round the next clustering is done on a subspace orthogonal
to the clustercenters of the current clustering.
To speed up the whole procedure, Greg Hamerlys faster k-means
is utilized. Initilization can be done either classically (uniformly random)
or by using the K-Means++ scheme.


