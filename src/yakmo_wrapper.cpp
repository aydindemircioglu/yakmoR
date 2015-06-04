// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
//
// yakmoR
//
//
//
// Copyright (C) 2015  Aydin Demircioglu, aydin.demircioglu /at/ ini.rub.de
//
// This file is part of the yakmoR library for GNU R.
// It is made available under the terms of the GNU General Public
// License, version 2, or at your option, any later version,
// incorporated herein by reference.
//
// This program is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public
// License along with this program; if not, write to the Free
// Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA
//

#include <sstream>
#include <string.h>
#include <Rcpp.h>

#include "yakmo.h"


using namespace std;
using namespace Rcpp;
using namespace yakmo;


//'  kmeans using yakmo
//' 
//'  @param	X		data matrix 
//'  @param	k		number of clusters
//'  @param	verbose		verbose output?
//'

// [[Rcpp::export]]
List KMeans(NumericMatrix X, unsigned int k = 3, unsigned int iter = 100, bool verbose = false ) {

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
	
	// create a new opt structure here
	yakmo::option opt (0, NULL);
	yakmo::orthogonal_kmeans* m = new yakmo::orthogonal_kmeans (opt);
	opt.k = k;
	/*
	dist_t   dist;  // dist-type
	init_t   init;  //
	mutable uint  k;
	mutable uint  m;
	uint     iter;
	bool     random;
	bool     normalize;
	uint16_t output;
	uint     verbosity;
	mode_t   mode;
	option (int argc, char** argv) : com (argc ? argv[0] : "--"), train ("-"), model ("-"), test ("-"), dist (EUCLIDEAN), init (KMEANSPP), k (3), m (1), iter (100), 
	*/
	
	// first do a check for k and number of rows
	if (X.rows() <= opt.k) {
		stringstream s;
		s << "Not enough data points (obtained " << X.rows() << ") to create " << k << "clusters!\n"; 
		Rcpp::stop(s.str().c_str());
	}
	
	// convert to std::vector probably stupid, but for now its ok
	unsigned int examples = X.rows();
	for (size_t e = 0; e < examples; e++) {
		NumericMatrix::Row zzrow = X (e, _);
		std::vector<double> tmp (zzrow.begin(), zzrow.end());
		inputs.push_back(tmp);
	}

	std::vector <kmeans*> _kms;
	kmeans* km = new kmeans (opt);
	_kms.push_back (km);

	for (uint i = 1; i <= opt.iter; ++i) {
		Rcout << "iteration " << i << "\n";
		if (i >= 2) {
			kmeans* km_ = _kms.back (); // last of mohikans
			// project
			std::vector <kmeans::point_t>& point_ = km_->point ();
			for (std::vector <kmeans::point_t>::iterator it = point_.begin ();
				 it != point_.end (); ++it)
				 it->project (km_->centroid ()[it->id]);
			km_->clear_centroid ();
		}
		km->run ();
	}
	
	// dummy
	Rcpp::List rl = Rcpp::List::create (Rcpp::Named ("X", X) );
	return (rl);
}


