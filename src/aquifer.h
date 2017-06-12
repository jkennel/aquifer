
// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppParallel)]]
#ifndef __FFTWTOOLS_H__
#define __FFTWTOOLS_H__


#include <boost/math/special_functions/gamma.hpp>
#include <boost/math/special_functions/expint.hpp>
#include <boost/math/special_functions/erf.hpp>
#include <boost/math/special_functions/factorials.hpp>
#include <boost/math/special_functions/bessel.hpp>

#include <boost/function.hpp>

#include <RcppArmadillo.h>
#include <RcppParallel.h>
#include <fftw3.h>
#include <complex.h>
//#include <fftwtools.h>

using namespace RcppParallel;

void fft_r2c(int* n, double* data,
             fftw_complex *res, int* retHermConj);


double bessel_k(double u);

double bh_gamma_neg(double u, double a);
double bh_tgamma(double u, double a);
double gamma_der(double u, double a);
double exp_int_single(double u, double a=0);

Rcpp::NumericVector grf_coefficient(Rcpp::NumericVector flow_rate, double radius, double K, double thickness, double flow_dimension);
Rcpp::NumericVector grf_parallel( Rcpp::NumericVector u,double a);

Rcpp::NumericVector impulse_function(Rcpp::NumericVector& u, double flow_time_interval=1.0);

double hantush_epsilon(double radius, double leakage);
double hantush_well_single(double u, double b, int n_terms=10);
Rcpp::NumericVector hantush_well_parallel(Rcpp::NumericVector u, double b, int n_terms);
Rcpp::NumericVector hantush_time_parallel(double radius, double storativity, double transmissivity, double leakage, const Rcpp::NumericVector& time, const Rcpp::NumericVector& flow_rate, int flow_time_interval);

Rcpp::NumericVector grf_u_time(double radius, double storativity, double K, const Rcpp::NumericVector& time);
Rcpp::NumericVector theis_u_time(double radius, double storativity, double transmissivity, const Rcpp::NumericVector& time);
Rcpp::NumericVector theis_u_radius(const Rcpp::NumericVector& radius, double storativity, double transmissivity, double time);
Rcpp::NumericVector grf_time_parallel(double radius, double storativity, double transmissivity, const Rcpp::NumericVector& time, const Rcpp::NumericVector& flow_rate, int flow_time_interval, double flow_dimension=2);

Rcpp::NumericVector well_function_coefficient(Rcpp::NumericVector flow_rate, double transmissivity);
Rcpp::NumericVector well_function_convolve(int flow_time_interval, const Rcpp::NumericVector& u, const Rcpp::NumericVector& coefs);

// arma::colvec hantush_freq(double radius,double storativity,double transmissivity,double leakage, const Rcpp::NumericVector& time, const Rcpp::NumericVector& flow_rate, int n_terms);
// arma::colvec grf_freq(double radius,double storativity,double transmissivity,const Rcpp::NumericVector& time,const Rcpp::NumericVector& flow_rate, double flow_dimension=2);

double ogata_banks(double D, double v, double C0, double x, double t);

#endif
