#include "aquifer.h"


// [[Rcpp::export]]
double slug_cbp_laplace_single(double p,
                               double radius_casing,
                               double radius_screen,
                               double radius,
                               double storativity= 1e-5,
                               double transmissivity = 1e-2,
                               double head_0 = 1) {


  double alpha = (radius_screen * radius_screen * storativity) /
                 (radius_casing * radius_casing);

  double q  =  sqrt((p * storativity) / transmissivity);
  double bess_rs, bess_r;

  if (radius == radius_screen) {
    bess_rs =  bessel_k_single(radius_screen * q, 0);
    bess_r = bess_rs;
  } else {
    bess_rs =  bessel_k_single(radius_screen * q, 0);
    bess_r  =  bessel_k_single(radius * q, 0);
  }

  return(
    (radius_screen * storativity * head_0 * bess_r) /
    ((transmissivity * q) *
    ((radius_screen * q * bess_rs) +
    (2 * alpha * bessel_k_single(radius_screen * q, 1))))
  );

}



//==============================================================================
struct cbp_worker : public Worker
{
  // source vector

  const RMatrix<double> input;

  RMatrix<double> output;

  double radius_casing;
  double radius_screen;
  double radius;
  double storativity;
  double transmissivity;
  double head_0;

  cbp_worker(const Rcpp::NumericMatrix input,
             Rcpp::NumericMatrix output,
             double radius_casing,
             double radius_screen,
             double radius,
             double storativity= 1e-5,
             double transmissivity = 1e-2,
             double head_0 = 1)
    : input(input), output(output), radius_casing(radius_casing),
      radius_screen(radius_screen), radius(radius), storativity(storativity),
      transmissivity(transmissivity), head_0(head_0) {}

  // calculate the exponential integral
  void operator()(std::size_t begin_row, std::size_t end_row) {
    for (std::size_t i = begin_row; i < end_row; i++) {

      output[i] = slug_cbp_laplace_single(input[i],
                                          radius_casing,
                                          radius_screen,
                                          radius,
                                          storativity,
                                          transmissivity,
                                          head_0);
    }
  }


};

//==============================================================================
//' @title
//' slug_cbp_parallel
//'
//' @description
//' parallel boost function for the Bessel function
//'
//' @param p elapsed time since slug introduction
//' @param radius_casing radius of the casing where the water level is
//' @param radius_screen radius of the screened interval
//' @param radius radius from borehole center to calculate the response
//' @param storativity aquifer storativity
//' @param transmissivity aquifer transmissivity
//' @param head_0 height of introduced head change
//'
//' @return cbp solution
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericMatrix slug_cbp_parallel(Rcpp::NumericMatrix p,
                                      double radius_casing,
                                      double radius_screen,
                                      double radius,
                                      double storativity = 1e-5,
                                      double transmissivity = 1e-2,
                                      double head_0 = 1) {

  Rcpp::NumericMatrix output(p.nrow(), p.ncol());

  cbp_worker cbp(p,
                 output,
                 radius_casing,
                 radius_screen,
                 radius,
                 storativity,
                 transmissivity,
                 head_0);

  RcppParallel::parallelFor(0, p.length(), cbp);

  return(output);
}


/*** R
library(microbenchmark)

tmp <- matrix(rep(1.0, 2e6), ncol = 1000)

microbenchmark(
  slug_cbp_parallel(tmp,
                    radius_casing = 7.6,
                    radius_screen = 7.6,
                    radius = 7.6,
                    storativity = 1e-5,
                    transmissivity = 1e-2,
                    head_0 = 1),
  .slug_cbp_laplace(tmp,
                    radius_casing = 7.6,
                    radius_screen = 7.6,
                    radius = 7.6,
                    storativity = 1e-5,
                    transmissivity = 1e-2,
                    head_0 = 1),
  times = 10
)

*/
