// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// orthoKMeansTrainCpp
List orthoKMeansTrainCpp(Rcpp::NumericMatrix x, bool random, bool verbose);
RcppExport SEXP yakmoR_orthoKMeansTrainCpp(SEXP xSEXP, SEXP randomSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type random(randomSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(orthoKMeansTrainCpp(x, random, verbose));
    return __result;
END_RCPP
}
// orthoKMeansPredictCpp
List orthoKMeansPredictCpp(NumericMatrix x, std::vector <NumericMatrix> centers, bool verbose);
RcppExport SEXP yakmoR_orthoKMeansPredictCpp(SEXP xSEXP, SEXP centersSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::vector <NumericMatrix> >::type centers(centersSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(orthoKMeansPredictCpp(x, centers, verbose));
    return __result;
END_RCPP
}
