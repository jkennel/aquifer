#include "aquifer.h"

// Ogata, A., Banks, R.B., 1961. A solution of the differential equation of
// longitudinal dispersion in porous media. U. S. Geol. Surv. Prof. Pap. 411-A.
//
// 1-D
// infinite source
// uniform flow
// constant parameters
// no decay
// no retardation
//==============================================================================
//' @title
//' Ogata-Banks solution for 1-D flow.
//'
//' @description
//' Ogata, A., Banks, R.B., 1961. A solution of the differential equation of
//' longitudinal dispersion in porous media. U. S. Geol. Surv. Prof. Pap. 411-A.
//' 1-D, infinite source, uniform flow, constant parameters, no decay, no retardation
//'
//' @param D diffusion coefficient
//' @param v double velocity
//' @param C0 double concentration
//' @param x double x position
//' @param t double time
//'
//' @return ogata banks solution
//'
//' @export
//'
// [[Rcpp::export]]
double ogata_banks(double D, double v, double C0, double x, double t) {


  return 0.5*C0 * (erfc((x-v*t) / (2*sqrt(D*t))) +
                   exp(v*x/D) * erfc((x+v*t) / (2*sqrt(D*t))));


}
