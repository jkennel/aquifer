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
double ogata_banks_ind(double D, double v, double C0, double x,
                                    double t) {

  return 0.5*C0 * (erfc((x-v*t) / (2*sqrt(D*t))) +
                   exp(v*x/D) * erfc((x+v*t) / (2*sqrt(D*t))));

}


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
//' Ogata-Banks solution for 1-D flow (vectorized).
//'
//' @description
//' Ogata, A., Banks, R.B., 1961. A solution of the differential equation of
//' longitudinal dispersion in porous media. U. S. Geol. Surv. Prof. Pap. 411-A.
//' 1-D, infinite source, uniform flow, constant parameters, decay, retardation
//'
//' To have values match the excel sheet
//' https://www.civil.uwaterloo.ca/jrcraig/pdf/OgataBanks.xlsm the decay
//' coefficient needs to be scaled by the retardation coefficient.
//'
//' @param D doublediffusion coefficient
//' @param R double retardation coefficient
//' @param decay double decay coefficient
//' @param v double velocity
//' @param C0 double concentration
//' @param x vector x position
//' @param t vector time
//'
//' @return ogata banks solution each row is an x value and each column is a time
//'
//' @export
//'
// [[Rcpp::export]]
arma::mat ogata_banks(double D, double R, double decay,
                      double v, double C0,
                        arma::vec x,
                        arma::rowvec t) {

  int n_x = x.n_elem;
  int n_t = t.n_elem;

  double B = sqrt(pow((v / (2 * D)), 2) + (decay * R / D));
  double term = sqrt(pow((v / R), 2) + (4 * decay * D / R));

  arma::mat output(n_x, n_t);

  for (int i = 0; i < n_t; i++) {
    output.col(i) = x;
  }


  output = 0.5 * C0 * exp(v * output / (2 * D)) %
    ((exp(-B * output) %
    arma::erfc((output.each_row() - term * t).each_row() / (2 * sqrt(D * t / R)))) +
    (exp(B * output) %
    arma::erfc((output.each_row() + term * t).each_row() / (2 * sqrt(D * t / R)))));

  return(output);

}




/*** R

D <- 0.1
R <- 1
decay <- 0
v <- 0.1
C0 <- 1
x <- 1:100
t <- 1:100

ogata_banks(D = D, R = R, decay = decay,
            v = v, C0 = C0, x = x,
            t = t)

*/

