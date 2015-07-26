# yakmoR

yakmoR is a simple wrapper for the K-Means C++ library 
(see www.tkl.iis.u-tokyo.ac.jp/~ynaga/yakmo/) developed
by Naoki Yoshinaga.

yakmoR implements orthogonal K-Means. It can work in several rounds.
In the first round, a normal K-Means is applied to the data.
In each subsequent round, the next clustering is done on a subspace orthogonal
to the centroids of the last clustering. This way one produces different
views on the data.
To speed up the whole procedure, Greg Hamerlys faster K-Means
is utilized. Initilization can be done either classically (uniformly random)
or by using the K-Means++ scheme.


# Note

To circumvent platform-dependent floating point problems, there are some differences to the original yakmo package:

- The different random number generators are replaced by the R::unif random number generator
- Compiling is done with the -ffloat-store flag. This prevents having different precision on different platforms (32bit vs 64bit)
- The projection is rounded to 14 digits precision. 

Therefore the results are not directly comparable to the original yakmo package.




# Changelist

-v0.1.0: Initial release.
