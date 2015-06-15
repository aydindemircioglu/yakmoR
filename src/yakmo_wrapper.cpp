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
//'  @param	x		data matrix 
//'  @param	k		number of clusters
//'  @param	iter	numer of iterations in one round
//'  @param	rounds		number of rounds (orthogonal views)
//'  @param	initType		centroid initialization via Random or KMeans++
//'  @param	random		use random or pseudo-random (seeded) generator?
//'  @param	verbose		verbose output?
//'
//'  @return	a list consisting of
//'	centers	these are the resulting centroids of the kmean algorithm (as a std::vector of NumericMatrix)
//'	cluster 	these are the labels for the resulting clustering (as a std::vector of NumericVector)
//'	obj			this is a vector with the final objective value for each round
//'
// [[Rcpp::export]]
List orthoKMeansTrainCpp (Rcpp::NumericMatrix x = Rcpp::NumericMatrix(), 
						  unsigned int k = 3, 
						  unsigned int iter = 100, 
						  unsigned int rounds = 1, 
						  std::string initType = "Random",
						  bool random = true, 
						  bool verbose = false) 
{
	Rcout << "START\n";
	
	// check parameter
	if ((initType != "Random") && (initType != "KMeans++"))
		stop ("Unknown initialization type of centroids.");
	
	// initalize the options
	if (verbose == true) {
		Rcout << "Parameters:\n";
		Rcout << "\trandom: \t" << random << "\n";
		Rcout<<"\tk: \t\t" << k << "\n";
 		Rcout<<"\titerations: \t" << iter<< "\n";
		Rcout<<"\trounds: \t\t" << rounds << "\n";
		Rcout<<"\tinitialization: \t" << initType << "\n";
	}

	// temp stringstream
	stringstream tmpS;
	
	
	
	// create a new opt structure here
	yakmo::option opt (0, NULL);
	opt.k = k;
	opt.iter = iter;
	opt.m = rounds;
	opt.random = random;
	opt.init = RANDOM;
	if (initType == "KMeans++")
		opt.init = KMEANSPP;
	
	
	if (verbose == true) 
		opt.verbosity = 1;

	// first do a check for k and number of rows
	if (x.rows() <= opt.k) {
		stringstream s;
		s << "Not enough data points (obtained " << x.rows() << ") to create " << k << "clusters!\n"; 
		Rcpp::stop(s.str().c_str());
	}
	
	// container for all the kmeans
	std::vector <kmeans*> _kms;
	
	// fill  current kmean object with data
	kmeans* km = new kmeans (opt);

	// FIXME: do we really need this?
	kmeans* shadow = new kmeans (opt);
	
	
	// TODO: actually, we would rather not convert, but then we need to change yakmo
	// and for now i do not want that.
	for (size_t e = 0; e < x.rows(); e++) {
		tmpS.str(std::string());
		
		for (size_t j = 0; j < x.cols(); j++) {
			tmpS << j << ":" << std::setprecision(16) << x (e, j);
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
	std::vector<NumericMatrix> centers;

	// extract clusters
	for (uint i = 0; i < _kms.size (); ++i) {
		const std::vector <kmeans::centroid_t>& centroid = _kms[i]->centroid ();
		NumericMatrix cc (centroid.size (), x.cols());
		for (uint j = 0; j < centroid.size (); ++j) {
			NumericVector tmpN(x.rows());
			tmpN = centroid[j].print();
			cc (j, _) = tmpN;
		}
		centers.push_back (cc);
	}

	
	
	// create labels for training set 
	std::vector<NumericVector> cluster;
	//NumericVector cluster (x.rows());
	
	// we need to go through all models even if user only wants
	// the last one as we somehow change the dataset?

	// get back centroids
	std::vector<kmeans::point_t> &pts = shadow -> point();
	for (uint i = 0; i < _kms.size (); ++i) {
		kmeans* km =_kms[i];
		
		km->decompress ();
		NumericVector cc (x.rows());
		
		for (uint j = 0; j < pts.size (); ++j) {
			kmeans::point_t p = pts[j];
			p.shrink (km->nf ());
			p.set_closest (km->centroid (), opt.dist);
			cc[j] = p.id;
			p.project (km->centroid ()[p.id]);
		} 
		cluster.push_back (cc); 
	}

	// return list
	Rcpp::List rl = Rcpp::List::create (
		Rcpp::Named ("centers", centers),
		Rcpp::Named ("cluster", cluster),
		Rcpp::Named ("obj", obj)
	//	Rcpp::Named ("dim", _kms.back()->nf()) 
	);
	
		
	return (rl);
}





//'  K-Means prediction using yakmo library
//' 
//'  @param	x		data matrix 
//'  @param	centers	centers FIXME
//'  @param	verbose		verbose output?
//'
//'  @return	a list consisting of
//'	centers	these are the resulting centroids of the kmean algorithm
//'	cluster 	these are the labels for the resulting clustering
//'	obj			this is a vector with the final objective value for each round
//'	dim			dimension of the input space (=dim of centroids)
//'	allcenters	this is the list of centroids, one matrix of centroids for each round
//'	allcluster		this is the list of labels, one vector for each round
//'
// [[Rcpp::export]]
List orthoKMeansPredictCpp (NumericMatrix x, 
				   std::vector <NumericMatrix> centers,
				   bool verbose = false	) {
	
	// the algorithm needs the following three variables
	// m, k, nf
	// we do not need m as we know the number of models in the list
	unsigned int k = 8;

	
	// create an option object
	yakmo::option opt (0, NULL);
	opt.k = k;
	opt.m = centers.size();

	if (verbose == true) {
		Rcout << "Parameters:\n";
		Rcout<<"\tk: \t\t" << k << "\n";
	}
	
	
	// temp stringstream
	stringstream tmpS;
	
	kmeans* shadow = new kmeans (opt);

	
	// container for all the kmeans
	std::vector <kmeans*> _kms;
	
	
	std::vector <kmeans::node_t> body;
	for (uint i = 0; i < opt.m; ++i) {
		// create a new kmeans object
		kmeans* km = new kmeans (opt);
//		km->nf () = nf;
// FIXME
		// TODO: actually, we would rather not convert, but then we need to change yakmo
		// and for now i do not want that.
		
		for (size_t e = 0; e < centers[i].rows(); e++) {
			tmpS.str(std::string());
			
			for (size_t j = 0; j < x.cols(); j++) {
				tmpS << j << ":" << std::setprecision(16) << x (e, j);
				tmpS << " ";
			}
			// get size (dont need to replay)
			int size = tmpS.str().length();
			
			char *cstr = new char [tmpS.str().length()+1];
			std::strcpy (cstr, tmpS.str().c_str());
			
			char *ex(cstr);
			char *ex_end (cstr + tmpS.str().length() );
			kmeans::point_t p = kmeans::read_point (ex, ex_end, body, opt.normalize);
			km->push_centroid (p, true); // delegated
			
			delete[] cstr;
		}
		
		_kms.push_back (km);
	}

	
	// now the same thing for the test data
	
	
	// TODO: actually, we would rather not convert, but then we need to change yakmo
	// and for now i do not want that.
	for (size_t e = 0; e < x.rows(); e++) {
		tmpS.str(std::string());
		
		for (size_t j = 0; j < x.cols(); j++) {
			tmpS << j << ":" << std::setprecision(16) << x (e, j);
			tmpS << " ";
		}
		// get size (dont need to replay)
		int size = tmpS.str().length();
		
		char *cstr = new char [tmpS.str().length()+1];
		std::strcpy (cstr, tmpS.str().c_str());
		
		char *ex(cstr);
		char *ex_end (cstr + tmpS.str().length() );
		_kms.front()->set_point (ex, ex_end, opt.normalize);
		shadow->set_point (ex, ex_end, opt.normalize);
		
		delete[] cstr;
	}
	
		
//	_kms.back ()->compress ();
	// _kms.back ()->clear_point ();
	
	
	// do we want to save all intermediate models?
	Rcpp::List allcluster;
	Rcpp::List allcenters;
	
	
	// create labels for training set 
	NumericVector cluster (x.rows());
	
	// we need to go through all models even if user only wants
	// the last one as we somehow change the dataset?
	
	// get back centroids
	std::vector<kmeans::point_t> &pts = shadow -> point();
	for (uint i = 0; i < _kms.size (); ++i) {
		kmeans* km =_kms[i];
		
		km->decompress ();
		NumericVector cc (x.rows());
		
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
	Rcpp::List rl = Rcpp::List::create ();
	rl["allcluster"] = allcluster;
	
	return (rl);
}


