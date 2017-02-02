#include "aquifer.h"

//==============================================================================
//' @title
//' grf_time_parallel
//'
//' @description
//' Parallel convolution of Theis well function and flow rates in the time domain.
//' Time series needs to be regularily spaced.
//'
//' @param radius distance to monitoring interval
//' @param storativity aquifer storativity
//' @param transmissivity aquifer transmissivity
//' @param time prediction times
//' @param flow_rate well flow rates
//' @param flow_time_interval time between flow rate measurements in samples
//' @param flow_dimension time between flow rate measurements in samples
//'
//' @return theis solution for multiple pumping scenario
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericVector grf_time_parallel(double radius,
                                      double storativity,
                                      double transmissivity,
                                      const Rcpp::NumericVector& time,
                                      const Rcpp::NumericVector& flow_rate,
                                      int flow_time_interval,
                                      double flow_dimension) {
  double v = 1 - flow_dimension/2;

  int n = time.size();
  int n_q = flow_rate.size();

  const Rcpp::NumericVector coefs = well_function_coefficient(flow_rate,  transmissivity);

  Rcpp::NumericVector u = Rcpp::NumericVector(n);
  Rcpp::NumericVector s = Rcpp::NumericVector(n);

  // calculate the Well function
  u = theis_u_time(radius, storativity, transmissivity, time);
  u = grf_parallel(u, v);

  // calculate the pulse
  u = impulse_function(u, flow_time_interval);

  s = well_function_convolve(flow_time_interval, u, coefs);

  return s;
}


//==============================================================================
//' @title
//' hantush_time_parallel
//'
//' @description
//' Parallel convolution of hantush well function and flow rates in the time domain.
//' Time series needs to be regularily spaced.
//'
//' @param radius distance to monitoring interval
//' @param storativity aquifer storativity
//' @param transmissivity aquifer transmissivity
//' @param leakage hantush leakage
//' @param time prediction times
//' @param flow_rate well flow rates
//' @param flow_time_interval time between flow rate measurements in samples
//' @param n_terms number of terms to use in Hantush solution.  More is more precise but slower.
//'
//' @return hantush jacob solution for multiple pumping scenario
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericVector hantush_time_parallel(double radius,
                                          double storativity,
                                          double transmissivity,
                                          double leakage,
                                          const Rcpp::NumericVector& time,
                                          const Rcpp::NumericVector& flow_rate,
                                          int flow_time_interval,
                                          int n_terms) {

  int n = time.size();
  int n_q = flow_rate.size();
  double b = hantush_epsilon(radius, leakage);

  const Rcpp::NumericVector coefs = well_function_coefficient(flow_rate,  transmissivity);

  Rcpp::NumericVector u = Rcpp::NumericVector(n);
  Rcpp::NumericVector s = Rcpp::NumericVector(n);

  // calculate the Well function
  u = theis_u_time(radius, storativity, transmissivity, time);
  u = hantush_well_parallel(u, b, n_terms);

  // calculate the pulse
  u = impulse_function(u, flow_time_interval);

  s = well_function_convolve(flow_time_interval, u, coefs);

  return s;
}


// FFTW is about 3 times faster-------------------------------------------------

// //==============================================================================
// //' @title
// //' hantush_freq
// //'
// //' @description
// //' Parallel convolution of hantush well function and flow rates in the frequency domain.
// //' Time series needs to be regularily spaced.
// //'
// //' @param radius distance to monitoring interval
// //' @param storativity aquifer storativity
// //' @param transmissivity aquifer transmissivity
// //' @param leakage hantush leakage
// //' @param time prediction times
// //' @param flow_rate well flow rates
// //' @param n_terms number of terms to use in Hantush solution.  More is more precise but slower.
// //'
// //' @return hantush jacob solution for multiple pumping scenario
// //'
// //'
// //' @export
// //'
// // [[Rcpp::export]]
// arma::colvec hantush_freq(double radius,
//                           double storativity,
//                           double transmissivity,
//                           double leakage,
//                           const Rcpp::NumericVector& time,
//                           const Rcpp::NumericVector& flow_rate,
//                           int n_terms) {
//
//   int n = time.size();
//   int n_q = flow_rate.size();
//   double b = hantush_epsilon(radius, leakage);
//
//   Rcpp::NumericVector coefs = well_function_coefficient(flow_rate,  transmissivity);
//
//   Rcpp::NumericVector u = Rcpp::NumericVector(n);
//   //Rcpp::NumericVector s = Rcpp::NumericVector(n);
//   arma::colvec s(n);
//
//   // calculate the Well function
//   u = theis_u_time(radius, storativity, transmissivity, time);
//   u = hantush_well_parallel(u, b, n_terms);
//
//   // calculate the pulse
//   u = impulse_function(u, 1);
//
//   arma::colvec y(u.begin(), u.size(), false);
//   arma::colvec z(coefs.begin(), coefs.size(), false);
//
//   s = arma::real(arma::ifft(arma::fft(z) % arma::fft(y)));
//
//   return s;
// }
//
//
// //==============================================================================
// //' @title
// //' grf_freq
// //'
// //' @description
// //' Parallel convolution of Theis well function and flow rates in the time domain.
// //' Time series needs to be regularily spaced.
// //'
// //' @param radius distance to monitoring interval
// //' @param storativity aquifer storativity
// //' @param transmissivity aquifer transmissivity
// //' @param time prediction times
// //' @param flow_rate well flow rates
// //' @param flow_dimension time between flow rate measurements in samples
// //'
// //' @return theis solution for multiple pumping scenario
// //'
// //'
// //' @export
// //'
// // [[Rcpp::export]]
// arma::colvec grf_freq(double radius,
//                       double storativity,
//                       double transmissivity,
//                       const Rcpp::NumericVector& time,
//                       const Rcpp::NumericVector& flow_rate,
//                       double flow_dimension) {
//
//   double v = 1 - flow_dimension/2;
//
//   int n = time.size();
//   int n_q = flow_rate.size();
//
//   Rcpp::NumericVector coefs = well_function_coefficient(flow_rate,  transmissivity);
//
//   Rcpp::NumericVector u = Rcpp::NumericVector(n);
//   arma::colvec s(n);
//
//   // calculate the Well function
//   u = theis_u_time(radius, storativity, transmissivity, time);
//   u = grf_parallel(v, u);
//
//   // calculate the pulse
//   u = impulse_function(u, 1);
//
//   arma::colvec y(u.begin(), u.size(), false);
//   arma::colvec z(coefs.begin(), coefs.size(), false);
//
//   s = arma::real(arma::ifft(arma::fft(z) % arma::fft(y)));
//
//   return s;
// }
