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
#include <iomanip>
#include <Rcpp.h>

#include "yakmo.h"


using namespace std;
using namespace Rcpp;
using namespace yakmo;



//'  K-Means using yakmo library
//' 
//'  @param	X		data matrix 
//'  @param	k		number of clusters
//'  @param	iter	numer of iterations in one round
//'  @param	m		number of rounds
//'  @param	verbose		verbose output?
//'  @param	allmodels	save all models of each round?
//'
//'  @return	a list consisting of
//'	centers	these are the resulting centroids of the kmean algorithm
//'	cluster 	these are the labels for the resulting clustering
//'	obj			this is a vector with the final objective value for each round
//'	dim			dimension of the input space (=dim of centroids)
//'	allcenters	this is the list of centroids, one matrix of centroids for each round
//'	allcluster		this is the list of labels, one vector for each round
//'
// (TODO: extract as some kind of prediction function?)
//
// [[Rcpp::export]]
List KMeans(NumericMatrix X, unsigned int k = 3, unsigned int iter = 100, unsigned int m = 1, bool verbose = false, bool allmodels = false ) {
	// temp stringstream
	stringstream tmpS;
	
	// initalize the options
	if (verbose == true) {
		Rcout << "Parameters:\n";
		Rcout<<"\tk: \t\t" << k << "\n";
 		Rcout<<"\titerations: \t" << iter<< "\n";
		Rcout<<"\tm: \t\t" << m << "\n";
	}
	
	// create a new opt structure here
	yakmo::option opt (0, NULL);
	opt.k = k;
	opt.iter = iter;
	opt.m = m;
	
	if (verbose == true) 
		opt.verbosity = 1;
	/*
	dist_t   dist;  // dist-type
	init_t   init;  //
	bool     random;
	bool     normalize;
	uint16_t output;
	mode_t   mode;
	option (int argc, char** argv) : com (argc ? argv[0] : "--"), train ("-"), model ("-"), test ("-"), dist (EUCLIDEAN), init (KMEANSPP), k (3), m (1), iter (100), 
	*/
	
	//yakmo::orthogonal_kmeans* m = new yakmo::orthogonal_kmeans (opt);
	
	// first do a check for k and number of rows
	if (X.rows() <= opt.k) {
		stringstream s;
		s << "Not enough data points (obtained " << X.rows() << ") to create " << k << "clusters!\n"; 
		Rcpp::stop(s.str().c_str());
	}
	
	// container for all the kmeans
	std::vector <kmeans*> _kms;
	
	// fill  current kmean object with data
	kmeans* km = new kmeans (opt);

	kmeans* shadow = new kmeans (opt);
	
	
	// TODO: actually, we would rather not convert, but then we need to change yakmo
	// and for now i do not want that.
	for (size_t e = 0; e < X.rows(); e++) {
		tmpS.str(std::string());
		
		for (size_t j = 0; j < X.cols(); j++) {
			tmpS << j << ":" << std::setprecision(16) << X (e, j);
				tmpS << " ";
		}
		// get size (dont need to replay)
		int size = tmpS.str().length();
		
		char *cstr = new char [tmpS.str().length()+1];
		std::strcpy (cstr, tmpS.str().c_str());
		
		char *ex(cstr);
		char *ex_end (cstr + tmpS.str().length() );
		km->set_point (ex, ex_end, opt.normalize);

		// just a container to the raw dataset we use below there
		// TODO: if we have an extra predict function, maybe
		// we do not need this here?
		shadow->set_point (ex, ex_end, opt.normalize);
		
		delete[] cstr;
	}
	
	// add it to our vector
	_kms.push_back (km);
	
	
	// return values
	Rcpp::NumericVector obj (opt.m);
	
	for (uint i = 1; i <= opt.m; ++i) {
		if (verbose) Rcout << "kmeans #" << i << "\n";
		if (i >= 2) {
			kmeans* km_ = _kms.back (); // last of mohikans
			// project
			std::vector <kmeans::point_t>& point_ = km_->point ();
			for (std::vector <kmeans::point_t>::iterator it = point_.begin ();
				 it != point_.end (); ++it)
				 it->project (km_->centroid ()[it->id]);

			km = new kmeans (opt);
			km_->delegate (km);
			km_->compress ();
			_kms.push_back (km);
			//	km_->clear_centroid ();
		}
		km->run ();
		obj[i-1] = km -> getObj();
	}

	_kms.back ()->compress ();
	// _kms.back ()->clear_point ();
	
	// do we want to save all intermediate models?
	Rcpp::List allcluster;
	Rcpp::List allcenters;

	// extract clusters
	NumericMatrix centers (opt.k, X.cols());
	if (allmodels == true) {
		// get back centroids
		for (uint i = 0; i < _kms.size (); ++i) {
			const std::vector <kmeans::centroid_t>& centroid = _kms[i]->centroid ();
			NumericMatrix cc (centroid.size (), X.cols());
			for (uint j = 0; j < centroid.size (); ++j) {
				NumericVector tmpN(X.rows());
				tmpN = centroid[j].print();
				cc (j, _) = tmpN;
			}
			tmpS.str("");
			tmpS << i;
			allcenters[tmpS.str()] = cc;
			// last centroids will be copied over
			if (i == _kms.size() - 1) {
				centers = cc;
			}
		}
	} else {
		const std::vector <kmeans::centroid_t>& centroid = _kms.back()->centroid ();
		for (uint j = 0; j < centroid.size (); ++j) {
			NumericVector tmpN(X.rows());
			tmpN = centroid[j].print();
			centers (j, _) = tmpN;
		}
	}

	
	
	// create labels for training set 
	NumericVector cluster (X.rows());
	
	// we need to go through all models even if user only wants
	// the last one as we somehow change the dataset?

	// get back centroids
	std::vector<kmeans::point_t> &pts = shadow -> point();
	for (uint i = 0; i < _kms.size (); ++i) {
		kmeans* km =_kms[i];
		
		km->decompress ();
		NumericVector cc (X.rows());
		
		for (uint j = 0; j < pts.size (); ++j) {
			kmeans::point_t p = pts[j];
			p.shrink (km->nf ());
			p.set_closest (km->centroid (), opt.dist);
			cc[j] = p.id;
			p.project (km->centroid ()[p.id]);
		} 
		tmpS.str("");
		tmpS << i;
		allcluster[tmpS.str()] = cc;
		// last centroids will be copied over
		if (i == _kms.size() - 1) {
			cluster = cc;
		}
	}

	// return list
	Rcpp::List rl = Rcpp::List::create (
		Rcpp::Named ("centers", centers),
		Rcpp::Named ("cluster", cluster),
		Rcpp::Named ("obj", obj), 
		Rcpp::Named ("dim", _kms.back()->nf()) );
	
	if (allmodels == true) {
		rl["allcenters"] = allcenters;
		rl["allcluster"] = allcluster;
	}
	
	return (rl);
}


