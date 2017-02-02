#include "aquifer.h"


struct parallel_convolve_time : public Worker
{
  // source vector

  int flow_time_interval;
  int n;
  const RVector<double> tc;
  const RVector<double> u;

  // accumulated value
  RVector<double> output;

  parallel_convolve_time(int flow_time_interval,
                         int n,
                         const Rcpp::NumericVector& tc,
                         const Rcpp::NumericVector& u,
                         Rcpp::NumericVector& output)
    : flow_time_interval(flow_time_interval), n(n), tc(tc), u(u), output(output) {
  }

  // calculate the convolution in the time domain
  void operator()(std::size_t begin_row, std::size_t end_row) {
    int ind;
    for (int i = begin_row; i < end_row; i++) {

      ind = i / flow_time_interval;

      if (ind >= n){
        ind = n-1;
      }

      // determine pumping regimes
      for (std::size_t j = 0; j <= ind; j++) {
        output[i] += tc[j] * u[i-(j*flow_time_interval)];
      }

    }
  }


};



//' @title
//' well_function_convolve
//'
//' @description
//' Parallel convolution in the time domain
//'
//' @param flow_time_interval time between flow rate measurements in samples
//' @param u well impuse function
//' @param coefs well coeffiicents
//'
//' @return impulse function for convolution
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericVector well_function_convolve(int flow_time_interval,
                                           const Rcpp::NumericVector& u,
                                           const Rcpp::NumericVector& coefs) {

  int n = u.size();
  int n_q = coefs.size();

  Rcpp::NumericVector s = Rcpp::NumericVector(n);

  parallel_convolve_time pc(flow_time_interval, n_q, coefs, u, s);

  RcppParallel::parallelFor(0, n, pc);

  return(s);
}




