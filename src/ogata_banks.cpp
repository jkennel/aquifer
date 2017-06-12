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
//' 1-D, infinite source, uniform flow, constant parameters, no decay, no retardation
//'
//' @param D diffusion coefficient
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
arma::mat ogata_banks(double D, double v, double C0,
                      arma::vec x,
                      arma::rowvec t) {

  int n_x = x.n_elem;
  int n_t = t.n_elem;

  arma::mat output(n_x, n_t);

  for (int i = 0; i < n_t; i++) {
    output.col(i) = x;
  }
  output = 0.5 * C0 * (arma::erfc((output.each_row() - v * t).each_row() /
    (2 * sqrt(D * t))) +
    exp(v * output / D) % arma::erfc((output.each_row() + v * t).each_row() / (2 * sqrt(D * t))));

  return(output);

}

# /*** R
# D <- 2
# v <- 1
# C0 <- 1
# x <- 1
# t <- 1
# library(microbenchmark)
# microbenchmark(
#   ogata_banks(D, v, C0, 1:100, 1:10),
#   #ogata_banks_ind(D, v, C0, 1, 1),
#   times = 100
# )
#
# tmp <- data.table(ogata_banks(D, v, C0, seq(1, 120, 2), seq(0, 80, 4)))
# tmp[, x := seq(1, 120, 2)]
# plot_ly(tmp, x = ~x, y = ~V1, mode='line') %>%
#   add_lines(x = ~x, y = ~V5, mode='line') %>%
#   add_lines(x = ~x, y = ~V10, mode='line') %>%
#   add_lines(x = ~x, y = ~V15, mode='line') %>%
#   add_lines(x = ~x, y = ~V20, mode='line')
#
#   */

//
// //==============================================================================
// struct ogata_banks_worker : public Worker
// {
//   // source vector
//
//   const RVector<double> input;
//   const RVector<double> t;
//   RMatrix<double> output;
//   double D;
//   double v;
//   double C0;
//
//   boost::function<double(double, double)> grf_p;
//
//
//   ogata_banks_worker(double D, double v, double C0,
//                      Rcpp::NumericVector& t,
//                      Rcpp::NumericVector& input,
//                      Rcpp::NumericMatrix& output)
//     : input(input), output(output), D(D), v(v), C0(C0), t(t) {
//   }
//
//   // calculate the exponential integral
//   void operator()(std::size_t begin_row, std::size_t end_row) {
//     for (std::size_t i = begin_row; i < end_row; i++) {
//         output[i] = ogata_banks_ind(D, v, C0, input[i], t);
//     }
//   }
//
//
// };
//
//
//
//
// //==============================================================================
// //' @title
// //' Ogata-Banks solution for 1-D flow.
// //'
// //' @description
// //' Ogata, A., Banks, R.B., 1961. A solution of the differential equation of
// //' longitudinal dispersion in porous media. U. S. Geol. Surv. Prof. Pap. 411-A.
// //' 1-D, infinite source, uniform flow, constant parameters, no decay, no retardation
// //'
// //' @param D diffusion coefficient
// //' @param v double velocity
// //' @param C0 double concentration
// //' @param x double x position
// //' @param t double time
// //'
// //' @return ogata banks solution
// //'
// //' @export
// //'
// // [[Rcpp::export]]
// Rcpp::NumericMatrix ogata_banks(double D, double v, double C0,
//                                 Rcpp::NumericVector x,
//                                 Rcpp::NumericVector t) {
//
//   int n_t = t.size();
//   int n_x = x.size();
//
//   //Rcpp::NumericVector output(n_x);
//   Rcpp::NumericMatrix output(n_x, n_t);
//
//   ogata_banks_worker ob(D, v, C0, t, x, output);
//   RcppParallel::parallelFor(0, n, pc);
//
//
//   return(output);
// }
//
//
//
// parallel_convolve_time pc(flow_time_interval, coefs, u, s);
//
// RcppParallel::parallelFor(0, n, pc);


