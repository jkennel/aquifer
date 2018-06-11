#include "aquifer.h"


// Barker Generalized Radial Flow ----------------------------------------------

//==============================================================================
//' @title
//' bh_tgamma
//'
//' @description
//' Approximate the gamma function using Boost
//'
//' @param u numeric to evaluate the gamma function
//' @param a numeric flow dimension parameter
//'
//' @return the result of the gamma function
//'
//' @export
// [[Rcpp::export]]
double bh_tgamma(double u, double a) {

  double ret;

  if (std::isfinite(u)) {
    ret = boost::math::tgamma(a, u);
  }
  else {
    ret = 0.0;
  }
  return(ret);
}


//==============================================================================
//' @title
//' bh_gamma_neg
//'
//' @description
//' Approximate the gamma function when a is negative using Boost
//'
//' @param u numeric to evaluate the gamma function
//' @param a numeric flow dimension parameter
//'
//' @return the result of the gamma function
//'
//' @export
// [[Rcpp::export]]
double bh_gamma_neg(double u, double a) {

  double ret;

  if (std::isfinite(u)) {
    ret = ((1 - boost::math::gamma_p(a+1, u)) * boost::math::tgamma(a+1) -
      pow(u, a) * exp(-u)) / a;
  }
  else {
    ret = 0.0;
  }

  return(ret);

}

//==============================================================================
//' @title
//' gamma_der
//'
//' @description
//' Derivative of the gamma function.
//'
//' @param u numeric to evaluate the gamma function
//' @param a numeric flow dimension parameter
//'
//' @return the result of the gamma function
//'
//' @export
// [[Rcpp::export]]
double gamma_der(double u, double a) {
  double ret;

  if (std::isfinite(u)) {
    ret = pow( u, a-1 ) / exp(u);
  }
  else {
    ret = 0.0;
  }
  return(ret);
}


//==============================================================================
//' @title
//' exp_int_single
//'
//' @description
//' Calculate the exponential integral using Boost
//'
//' @param u value of the Theis u
//' @param a not used (flow dimension)
//'
//' @return exponential integral
//'
//' @useDynLib aquifer
//' @import RcppParallel
//' @importFrom Rcpp evalCpp
//'
//' @export
//'
// [[Rcpp::export]]
double exp_int_single(double u, double a) {

    if (u == 0){
      u = R_PosInf;
    } else if (u > 40.0){
      u = 0;
    } else {
      u = -boost::math::expint(-u);
    }

  return(u);
}

//==============================================================================
struct grf_worker : public Worker
{
  // source vector

  const RVector<double> input;
  RVector<double> output;
  double a;

  boost::function<double(double, double)> grf_p;


  grf_worker(double a, Rcpp::NumericVector& input,
             Rcpp::NumericVector& output)
    : input(input), output(output), a(a) {
    // choose the appropriate function based on the flow dimension

    if(a==0){
      grf_p = &exp_int_single;
    } else if(a>0){
      grf_p = &bh_tgamma;
    } else {
      grf_p = &bh_gamma_neg;
    }
  }

  // calculate the exponential integral
  void operator()(std::size_t begin_row, std::size_t end_row) {
    for (std::size_t i = begin_row; i < end_row; i++) {
      output[i] = grf_p(input[i], a);
    }
  }


};

//==============================================================================
//' @title
//' grf_parallel
//'
//' @description
//' Parallel vector version of the exponential integral
//'
//' @param u value of the Theis u
//' @param a flow dimension
//'
//' @return exponential integral
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericVector grf_parallel(Rcpp::NumericVector u, double a) {

  int n = u.size();

  Rcpp::NumericVector output(n);

  grf_worker eip(a, u, output);

  RcppParallel::parallelFor(0, n, eip);

  return(output);
}





// Hantush ---------------------------------------------------------------------
// Hunt, B., 1977. Calculation of the leaky aquifer function. J. Hydrol., 33:179--183


//==============================================================================
//' @title
//' bessel_k
//'
//' @description
//' boost function for the Bessel function
//'
//' @param u value for bessel function
//'
//' @return result of the bessel function
//'
//'
//' @export
//'
// [[Rcpp::export]]
double bessel_k(double u) {

  u = boost::math::cyl_bessel_k(0, u);

  return(u);
}



//==============================================================================
//' @title
//' hantush_well_single
//'
//' @description
//' Result of the hantush well function
//'
//' @param u value of the Theis u
//' @param b the leakance
//' @param n_terms the number of terms used in the hantush approximation
//'
//'
//' @return hantush well function
//'
//'
//' @export
//'
// [[Rcpp::export]]
double hantush_well_single(double u, double b, int n_terms){

  double b_div_u = b/u;
  double out = 0;
  double en;

  if(b_div_u >= u){
    en = exp_int_single(b_div_u);

    for (int i = 0; i < n_terms; i++){
      out += en * (pow(-u, i) / boost::math::factorial<double>(i));
      en = (1.0/(i+1.0)) * (exp(-b_div_u) - b_div_u * en);
    }
    out = 2*bessel_k(2.0 * sqrt(b)) - out;

  } else {

    en = exp_int_single(u);
    for (int i = 0; i < n_terms; i++){
      out += en * (pow(-b_div_u, i) / boost::math::factorial<double>(i));
      en = (1.0/(i+1.0)) * (exp(-b_div_u) - b_div_u * en);
    }
  }

  return(out);
}



//==============================================================================
struct hantush_well_worker : public Worker
{
  // source vector

  const RVector<double> u;
  double b;
  RVector<double> output;
  int n_terms;

  hantush_well_worker(const Rcpp::NumericVector& u,
                      double b,
                      Rcpp::NumericVector& output,
                      int n_terms)
    : u(u), b(b), output(output), n_terms(n_terms) {
  }

  // calculate the exponential integral
  void operator()(std::size_t begin_row, std::size_t end_row) {
    for (std::size_t i = begin_row; i < end_row; i++) {
      output[i] = hantush_well_single(u[i], b, n_terms);
    }
  }


};

//==============================================================================
//' @title
//' hantush_well_single
//'
//' @description
//' Parallel version of the hantush well function
//'
//'
//' @param u (vector) value of the Theis u
//' @param b the leakance
//' @param n_terms the number of terms used in the hantush approximation
//'
//'
//' @return hantush well function
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericVector hantush_well_parallel(Rcpp::NumericVector u,
                                          double b,
                                          int n_terms) {

  int n = u.size();

  Rcpp::NumericVector output(n);

  hantush_well_worker hwp(u, b, output, n_terms);

  RcppParallel::parallelFor(0, n, hwp);

  return(output);
}



// Neumann ---------------------------------------------------------------------
// Moench  ---------------------------------------------------------------------


/*** R

# library(microbenchmark)
# n <- 1e6
# microbenchmark(exp_int_parallel_a(1:n/n),
#                exp_int_parallel(1:n/n),
#                times=5
# )
*/
