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
List orthoKMeansTrainCpp (
	Rcpp::NumericMatrix x,
	unsigned int rounds = 1,
	unsigned int k = 3,
	unsigned int iter = 100,
	unsigned int initType = 0,
	bool random = false,
	bool verbose = false
)
{

	// check parameter
	if ((initType != 0) && (initType != 1))
		stop ("Unknown initialization type of centroids.");
	
	// initalize the options
	if (verbose == true) {
		Rcout << "Parameters:\n";
		Rcout << "\trandom: \t" << random << "\n";
		Rcout<<"\tk: \t\t" << k << "\n";
 		Rcout<<"\titerations: \t" << iter<< "\n";
		Rcout<<"\trounds: \t" << rounds << "\n";
		Rcout<<"\tinitialization: " << initType << "\n";
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
	if (initType == 1)
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

	// get back centroids and nf vector (TODO:needed?)
	unsigned int nf;
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
		nf = (km -> nf());
	}
	
	// return list
	Rcpp::List rl = Rcpp::List::create (
		Rcpp::Named ("centers", centers),
		Rcpp::Named ("cluster", cluster),
		Rcpp::Named ("obj", obj),
		Rcpp::Named ("nf", nf)
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
				   unsigned int nf,
				   unsigned int k = 0,
				   bool verbose = false	) {
	
	// temp stringstream
	stringstream tmpS;

	// create an option object
	yakmo::option opt (0, NULL);
	opt.m = centers.size();
	opt.k = k;
	
	// initalize the options
	if (verbose == true) {
		Rcout << "Parameters:\n";
		Rcout<<"\tk: \t\t" << opt.k << "\n";		
		Rcout<<"\tm: \t\t" << opt.m << "\n";
	}
		
	// container for all the kmeans
	std::vector <kmeans*> _kms;
	
	
	// load data first
	std::vector <kmeans::node_t> body;
	for (uint i = 0; i < centers.size(); ++i) {
		// create a new kmeans object
		kmeans* km = new kmeans (opt);
		km -> nf() = nf;
		for (size_t e = 0; e < centers[i].rows(); e++) {
			tmpS.str(std::string());
			
			for (size_t j = 0; j < centers[i].cols(); j++) {
				tmpS << j << ":" << std::setprecision(16) << centers[i](e, j);
				tmpS << " ";
			}
			// get size (dont need to replay)
			int size = tmpS.str().length();
			
			char *cstr = new char [tmpS.str().length()+1];
			std::strcpy (cstr, tmpS.str().c_str());
			
			char *ex(cstr);
			char *ex_end (cstr + tmpS.str().length() );
			km->nf () = nf;
			kmeans::point_t p = kmeans::read_point (ex, ex_end, body, opt.normalize);
			km->push_centroid (p, true); // delegated
			
			delete[] cstr;
		}
		
		_kms.push_back (km);
	}
	
		
	std::vector <kmeans::point_t>     point;
	body.clear();
	
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
		point.push_back (kmeans::read_point (ex, ex_end, body, opt.normalize));
		
		delete[] cstr;
	}
	
	
	// create labels for training set 
	NumericMatrix cluster (x.rows(), centers.size());
	for (unsigned r = 0; r < centers.size(); r++) {
		if (verbose == TRUE) 
			Rcout << "Predicting round "<< r << ".\n";
		
		// get back centroids
		kmeans* km =_kms[ r ];
		
		km->decompress ();
		NumericVector cc (x.rows());
		
		for (uint j = 0; j < point.size (); ++j) {
			kmeans::point_t p = point[j];
			p.shrink (km -> nf());
			p.set_closest (km->centroid (), opt.dist);
			cc[j] = p.id;
			p.project (km->centroid ()[p.id]);
		} 
		cluster (_ ,r) = cc;
	}
	
	// return list
	Rcpp::List rl = Rcpp::List::create ();
	rl["cluster"] = cluster;
	
	return (rl);
}



