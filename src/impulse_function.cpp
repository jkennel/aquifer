#include "aquifer.h"

//==============================================================================
//' @title
//' impulse_function
//'
//' @description
//' Calculation of the impulse function from a well function.
//'
//' @param u well function
//' @param flow_time_interval time between flow rate measurements in samples
//'
//' @return impulse function for convolution
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericVector impulse_function(Rcpp::NumericVector& u, double flow_time_interval) {

  int n = u.size();

  // calculate the pulse
  for (int i = (n - flow_time_interval - 1); i > -1; i--) {
    u[i+flow_time_interval] -= u[i];
  }

  return u;
}
