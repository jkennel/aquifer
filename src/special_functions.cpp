#include "aquifer.h"

#include <boost/foreach.hpp>
#define foreach BOOST_FOREACH
// Barker Generalized Radial Flow ----------------------------------------------


//==============================================================================
//' @title
//' bessel_k_single
//'
//' @description
//' boost function for the Bessel function
//'
//' @param x value for bessel function
//' @param nu order
//'
//' @return result of the bessel function
//'
//'
//' @export
//'
// [[Rcpp::export]]
double bessel_k_single(double x, int nu) {

  return(boost::math::cyl_bessel_k(nu, x));

}



//' @title
//' bessel_i_gamma_term
//'
//' @description
//' Modified Bessel function of first kind order 1
//'
//' @param x \code{numeric} value to evaluate
//' @param v \code{numeric} value to evaluate
//'
//' @return bessel function result
//'
//'
//' @export
// [[Rcpp::export]]
double bessel_i_gamma_term(double jj, double v) {
  return(1.0 / (boost::math::tgamma((double)jj + 1.0) * boost::math::tgamma(v + 1.0 + (double)jj)));
}

//' @title
//' bessel_i_complex
//'
//' @description
//' Modified Bessel function of first kind order 1
//'
//' @param x \code{numeric} value to evaluate
//' @param v \code{numeric} value to evaluate
//'
//' @return bessel function result
//'
//'
//' @export
// [[Rcpp::export]]
arma::cx_double bessel_i_complex_new(arma::cx_double x,
                                     double v,
                                     arma::vec gamma_term) {

  arma::cx_double ret = arma::cx_double(0.0, 0.0);

  arma::cx_double t1 = pow(x / 2.0, v);

  for (int kk = 0; kk < gamma_term.n_elem; kk++) {

    ret = ret + t1 * gamma_term[kk] *
      pow((x / 2.0), (2.0 * (double)kk));

  }

  return ret;
}

//' @title
//' bessel_k_complex for complex values
//'
//' @description
//' Modified Bessel function of second kind order 0
//'
//' @param x \code{numeric} value to evaluate
//'
//' @return bessel function result
//'
//'
//' @export
// [[Rcpp::export]]
arma::cx_double bessel_k_complex_opt(const arma::cx_double x,
                                     arma::vec gamma_term_a,
                                     arma::vec gamma_term_b) {

  double v = 1e-8;

  arma::cx_double result;

  result = M_PI_2 * (bessel_i_complex_new(x, -v, gamma_term_b) -
            bessel_i_complex_new(x, v, gamma_term_a)) /
            sin(v * M_PI);

  return result;

}








//' @title
//' bessel_i_complex
//'
//' @description
//' Modified Bessel function of first kind order 1
//'
//' @param x \code{numeric} value to evaluate
//' @param v \code{numeric} value to evaluate
//'
//' @return bessel function result
//'
//'
//' @export
// [[Rcpp::export]]
arma::cx_double bessel_i_complex(arma::cx_double x, double v) {

  arma::cx_double ret = arma::cx_double(0.0, 0.0);

  arma::cx_double t1 = pow(x / 2.0, v);

  for (int k = 0; k < 13; k++) {
    // Rcpp::Rcout << "The value" << (boost::math::tgamma((double)k + 1.0) * boost::math::tgamma(v + 1.0 + (double)k)) << std::endl;

    ret = ret + t1 * 1.0 / (boost::math::tgamma((double)k + 1.0) * boost::math::tgamma(v + 1.0 + (double)k)) *
      pow((x / 2.0), (2.0 * (double)k));
  }


  return ret;
}







//' @title
//' bessel_k_complex for complex values
//'
//' @description
//' Modified Bessel function of second kind order 0
//'
//' @param x \code{numeric} value to evaluate
//'
//' @return bessel function result
//'
//'
//' @export
// [[Rcpp::export]]
arma::cx_rowvec bessel_k_complex(const arma::cx_rowvec &x) {

  double v = 1e-8;
  double denom = sin(v * M_PI) / (M_PI_2) ;

  int n = x.n_elem;
  arma::cx_rowvec result(n);

  int n_terms = 13;
  arma::rowvec gamma_term_a(n_terms);
  arma::rowvec gamma_term_b(n_terms);

  for (int j = 0; j < n_terms; j++) {
    gamma_term_a(j) = bessel_i_gamma_term(j, v);
    gamma_term_b(j) = bessel_i_gamma_term(j, -v);
  }

  for (int k = 0; k < n; k++) {
    result[k]  = (bessel_i_complex_new(x[k], -v, gamma_term_b) - bessel_i_complex_new(x[k], v, gamma_term_a)) / denom;
    //result[k] = (bessel_i_complex(x[k], -v) - bessel_i_complex(x[k], v)) / denom;

    // Rcpp::Rcout << "The value 1 " << result[k] << std::endl;
    // Rcpp::Rcout << "The value 2 " << result2[k] << std::endl;
  }
  return result;

}

//' @title
//' bessel_k_complex_single for complex values
//'
//' @description
//' Modified Bessel function of second kind order 0
//'
//' @param x \code{numeric} value to evaluate
//'
//' @return bessel function result
//'
//'
//' @export
// [[Rcpp::export]]
arma::cx_double bessel_k_complex_single(arma::cx_double x) {

  double v = 1e-8;

  return (M_PI_2) * (bessel_i_complex(x,-v) - bessel_i_complex(x, v)) /
    sin(v * M_PI);

}



//==============================================================================
struct bessel_worker : public Worker
{
  // source vector

  const RMatrix<double> input;

  RMatrix<double> output;

  int nu;

  bessel_worker(const Rcpp::NumericMatrix input,
                Rcpp::NumericMatrix output,
                int nu)
    : input(input), output(output), nu(nu) {}

  // calculate the exponential integral
  void operator()(std::size_t begin_row, std::size_t end_row) {
    for (std::size_t i = begin_row; i < end_row; i++) {
      output[i] = bessel_k_single(input[i], nu);
    }
  }


};

//==============================================================================
struct bessel_k0_complex_worker : public Worker
{
  // source vector

  const arma::cx_vec input;

  arma::cx_vec output;

  bessel_k0_complex_worker(const arma::cx_vec input,
                arma::cx_vec output)
    : input(input), output(output) {}

  // calculate the exponential integral
  void operator()(std::size_t begin_row, std::size_t end_row) {
    for (std::size_t i = begin_row; i < end_row; i++) {
      output[i] = bessel_k_complex_single(input[i]);
    }
  }


};

//==============================================================================
//' @title
//' bessel_k_parallel
//'
//' @description
//' parallel boost function for the Bessel function
//'
//' @param x value for bessel function
//' @param nu order
//'
//' @return bessel k
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericMatrix bessel_k_parallel(Rcpp::NumericMatrix x, int nu) {

  Rcpp::NumericMatrix output(x.nrow(), x.ncol());

  bessel_worker besselkp(x, output, nu);

  RcppParallel::parallelFor(0, x.length(), besselkp);

  return(output);
}

//==============================================================================
//' @title
//' bessel_k_complex_parallel
//'
//' @description
//' parallel boost function for the Bessel function
//'
//' @param x value for bessel function
//'
//' @return bessel k
//'
//'
//' @export
//'
// [[Rcpp::export]]
arma::cx_vec bessel_k_complex_parallel(arma::cx_vec x) {

  arma::cx_vec output(x.n_rows, x.n_cols);

  bessel_k0_complex_worker besselkp(x, output);

  RcppParallel::parallelFor(0, x.n_elem, besselkp);

  return(output);
}


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
    out = 2*bessel_k_single(2.0 * sqrt(b), 0) - out;

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
library(microbenchmark)
library(Bessel)
x <- matrix(rep(1, 100000), ncol =10)
x <- complex(real= rnorm(100), imaginary = rnorm(100))
microbenchmark(
  bessel_k_complex(x),
  BesselK(x, 0),
  times = 10
)




# library(microbenchmark)
# n <- 1e6
# microbenchmark(exp_int_parallel_a(1:n/n),
#                exp_int_parallel(1:n/n),
#                times=5
# )
*/
