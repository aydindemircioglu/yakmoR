
#include "yakmo.h"
#include <Rcpp.h>
using namespace Rcpp;

//'  kmeans using yakmo
//' 
//'  @param	X		data matrix 
//'  @param	k		number of clusters
//'  @param	verbose		verbose output?
//'
//'
// [[Rcpp::export]]
List kmeans(NumericMatrix X, unsigned int k, bool verbose = false ) {

	// initalize the options
	
	// convert to data structure
	
	
	if (verbose) Rcout << "Converting data.. " << std::endl;
	std::vector<std::vector <double> > inputs;
	
	if (verbose == true) {
		Rcout << "Parameters:\n";
		Rcout<<"\tk: \t\t" << k << "\n";
// 		Rcout<<"\tgamma: \t\t" << gamma << "\n";
// 		Rcout<<"\teps: \t\t" << epsilon << "\n";
	}
	
	// probably stupid, but for now its ok
	unsigned int examples = X.rows();
	for (size_t e = 0; e < examples; e++) {
		NumericMatrix::Row zzrow = X (e, _);
		std::vector<double> tmp (zzrow.begin(), zzrow.end());
		inputs.push_back(tmp);
	}
	
	// dummy
	Rcpp::List rl = Rcpp::List::create (Rcpp::Named ("X", X) );
	return (rl);
}
