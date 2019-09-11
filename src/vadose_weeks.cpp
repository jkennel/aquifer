#include "aquifer.h"

// [[Rcpp::export]]
double weeks_1979(double lag,
                  double D,
                  double L,
                  double precision = 1e-8,
                  bool inverse = true) {

  double d_term = M_PI * M_PI * D * lag / (4.0 * L * L);
  double term_val = 0.0;
  double ret = 0.0;
  double exp_val = 0.0;
  int m = 1;
  bool more_precise = TRUE;

  if(inverse) {
    if (d_term < 0.001) {
      return(0.0);
    }
  } else {
    if (d_term < 0.001) {
      return(1.0);
    }
  }


  while(more_precise) {
    exp_val  = -double(m * m) * d_term;
    term_val = std::pow(-1.0, (double(m) - 1.0) / 2.0) / double(m) * std::exp(exp_val);
    ret += term_val;
    m   += 2;
    more_precise = std::abs(term_val) > precision;
  }

  if(inverse) {
    ret = (1.0 - (4.0 / M_PI) * ret);
    if (ret < 0.0) {
      ret = 0.0;
    }
  } else {
    ret = ((4.0 / M_PI) * ret);
    if (ret > 1.0) {
      ret = 1.0;
    }
  }

  return(ret);

}


//==============================================================================
struct weeks_worker : public Worker
{
  // source vector

  const RVector<double> input;
  RVector<double> output;

  double D;
  double L;
  double precision;
  bool inverse;

  weeks_worker(const Rcpp::NumericVector input,
               Rcpp::NumericVector output,
               double D,
               double L,
               double precision,
               bool inverse)
    : input(input), output(output), D(D), L(L), inverse(inverse) {}

  // calculate the exponential integral
  void operator()(std::size_t begin_row, std::size_t end_row) {

    for (std::size_t i = begin_row; i < end_row; i++) {
      output[i] = weeks_1979(input[i], D, L, precision, inverse);
    }

  }


};

//==============================================================================
//' @title
//' vadose_response
//'
//' @description
//' weeks_1979 1-D air diffusivity
//'
//' @param time \code{numeric} value of elapsed time
//' @param D \code{numeric} unsaturated zone air diffusivity
//' @param L \code{numeric} unsaturated zone thickness
//' @param precision \code{numeric} precision of solution
//' @param inverse \code{logical} inverse water level relationship
//'
//' @return weeks 1979 model
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericVector vadose_response(const Rcpp::NumericVector time,
                                    double D,
                                    double L,
                                    double precision = 1e-08,
                                    bool inverse = true) {

  int n = time.size();

  Rcpp::NumericVector output(n);

  weeks_worker weeks(time, output, D, L, precision, inverse);

  RcppParallel::parallelFor(0, n, weeks);

  return(output);
}

/***R
#
time <- seq(0, 86400*4.5, 0.2)
D <- 0.0165
L <- 66.04
precision <- 1e-14

tmp <- vadose_response(time = as.numeric(0:43200), D = 0.20, L = 40, precision = 1e-10, inverse = FALSE)
plot(tmp, type='l')

system.time(
  w <- vadose_response(time, D, L, precision, inverse = FALSE)
)
*/
