// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// impulse_function
Rcpp::NumericVector impulse_function(Rcpp::NumericVector& u, double flow_time_interval);
RcppExport SEXP aquifer_impulse_function(SEXP uSEXP, SEXP flow_time_intervalSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type flow_time_interval(flow_time_intervalSEXP);
    rcpp_result_gen = Rcpp::wrap(impulse_function(u, flow_time_interval));
    return rcpp_result_gen;
END_RCPP
}
// ogata_banks
double ogata_banks(double D, double v, double C0, double x, double t);
RcppExport SEXP aquifer_ogata_banks(SEXP DSEXP, SEXP vSEXP, SEXP C0SEXP, SEXP xSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type D(DSEXP);
    Rcpp::traits::input_parameter< double >::type v(vSEXP);
    Rcpp::traits::input_parameter< double >::type C0(C0SEXP);
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(ogata_banks(D, v, C0, x, t));
    return rcpp_result_gen;
END_RCPP
}
// bh_tgamma
double bh_tgamma(double u, double a);
RcppExport SEXP aquifer_bh_tgamma(SEXP uSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(bh_tgamma(u, a));
    return rcpp_result_gen;
END_RCPP
}
// bh_gamma_neg
double bh_gamma_neg(double u, double a);
RcppExport SEXP aquifer_bh_gamma_neg(SEXP uSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(bh_gamma_neg(u, a));
    return rcpp_result_gen;
END_RCPP
}
// gamma_der
double gamma_der(double u, double a);
RcppExport SEXP aquifer_gamma_der(SEXP uSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(gamma_der(u, a));
    return rcpp_result_gen;
END_RCPP
}
// exp_int_single
double exp_int_single(double u, double a);
RcppExport SEXP aquifer_exp_int_single(SEXP uSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(exp_int_single(u, a));
    return rcpp_result_gen;
END_RCPP
}
// grf
Rcpp::NumericVector grf(double a, const Rcpp::NumericVector& u);
RcppExport SEXP aquifer_grf(SEXP aSEXP, SEXP uSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type u(uSEXP);
    rcpp_result_gen = Rcpp::wrap(grf(a, u));
    return rcpp_result_gen;
END_RCPP
}
// grf_parallel
Rcpp::NumericVector grf_parallel(double a, Rcpp::NumericVector u);
RcppExport SEXP aquifer_grf_parallel(SEXP aSEXP, SEXP uSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type u(uSEXP);
    rcpp_result_gen = Rcpp::wrap(grf_parallel(a, u));
    return rcpp_result_gen;
END_RCPP
}
// bessel_k
double bessel_k(double u);
RcppExport SEXP aquifer_bessel_k(SEXP uSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_k(u));
    return rcpp_result_gen;
END_RCPP
}
// hantush_well_single
double hantush_well_single(double u, double b, int n_terms);
RcppExport SEXP aquifer_hantush_well_single(SEXP uSEXP, SEXP bSEXP, SEXP n_termsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type n_terms(n_termsSEXP);
    rcpp_result_gen = Rcpp::wrap(hantush_well_single(u, b, n_terms));
    return rcpp_result_gen;
END_RCPP
}
// hantush_well
Rcpp::NumericVector hantush_well(const Rcpp::NumericVector& u, double b, int n_terms);
RcppExport SEXP aquifer_hantush_well(SEXP uSEXP, SEXP bSEXP, SEXP n_termsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type n_terms(n_termsSEXP);
    rcpp_result_gen = Rcpp::wrap(hantush_well(u, b, n_terms));
    return rcpp_result_gen;
END_RCPP
}
// hantush_well_parallel
Rcpp::NumericVector hantush_well_parallel(Rcpp::NumericVector u, double b, int n_terms);
RcppExport SEXP aquifer_hantush_well_parallel(SEXP uSEXP, SEXP bSEXP, SEXP n_termsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type n_terms(n_termsSEXP);
    rcpp_result_gen = Rcpp::wrap(hantush_well_parallel(u, b, n_terms));
    return rcpp_result_gen;
END_RCPP
}
// well_function_coefficient
Rcpp::NumericVector well_function_coefficient(Rcpp::NumericVector flow_rate, double transmissivity);
RcppExport SEXP aquifer_well_function_coefficient(SEXP flow_rateSEXP, SEXP transmissivitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type flow_rate(flow_rateSEXP);
    Rcpp::traits::input_parameter< double >::type transmissivity(transmissivitySEXP);
    rcpp_result_gen = Rcpp::wrap(well_function_coefficient(flow_rate, transmissivity));
    return rcpp_result_gen;
END_RCPP
}
// hantush_epsilon
double hantush_epsilon(double radius, double leakage);
RcppExport SEXP aquifer_hantush_epsilon(SEXP radiusSEXP, SEXP leakageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type leakage(leakageSEXP);
    rcpp_result_gen = Rcpp::wrap(hantush_epsilon(radius, leakage));
    return rcpp_result_gen;
END_RCPP
}
// grf_coefficient
Rcpp::NumericVector grf_coefficient(Rcpp::NumericVector flow_rate, double radius, double K, double thickness, double flow_dimension);
RcppExport SEXP aquifer_grf_coefficient(SEXP flow_rateSEXP, SEXP radiusSEXP, SEXP KSEXP, SEXP thicknessSEXP, SEXP flow_dimensionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type flow_rate(flow_rateSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type K(KSEXP);
    Rcpp::traits::input_parameter< double >::type thickness(thicknessSEXP);
    Rcpp::traits::input_parameter< double >::type flow_dimension(flow_dimensionSEXP);
    rcpp_result_gen = Rcpp::wrap(grf_coefficient(flow_rate, radius, K, thickness, flow_dimension));
    return rcpp_result_gen;
END_RCPP
}
// theis_u_time
Rcpp::NumericVector theis_u_time(double radius, double storativity, double transmissivity, const Rcpp::NumericVector& time);
RcppExport SEXP aquifer_theis_u_time(SEXP radiusSEXP, SEXP storativitySEXP, SEXP transmissivitySEXP, SEXP timeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type storativity(storativitySEXP);
    Rcpp::traits::input_parameter< double >::type transmissivity(transmissivitySEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type time(timeSEXP);
    rcpp_result_gen = Rcpp::wrap(theis_u_time(radius, storativity, transmissivity, time));
    return rcpp_result_gen;
END_RCPP
}
// well_function_convolve
Rcpp::NumericVector well_function_convolve(int flow_time_interval, const Rcpp::NumericVector& u, const Rcpp::NumericVector& coefs);
RcppExport SEXP aquifer_well_function_convolve(SEXP flow_time_intervalSEXP, SEXP uSEXP, SEXP coefsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type flow_time_interval(flow_time_intervalSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type u(uSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type coefs(coefsSEXP);
    rcpp_result_gen = Rcpp::wrap(well_function_convolve(flow_time_interval, u, coefs));
    return rcpp_result_gen;
END_RCPP
}
// grf_time_parallel
Rcpp::NumericVector grf_time_parallel(double radius, double storativity, double transmissivity, const Rcpp::NumericVector& time, const Rcpp::NumericVector& flow_rate, int flow_time_interval, double flow_dimension);
RcppExport SEXP aquifer_grf_time_parallel(SEXP radiusSEXP, SEXP storativitySEXP, SEXP transmissivitySEXP, SEXP timeSEXP, SEXP flow_rateSEXP, SEXP flow_time_intervalSEXP, SEXP flow_dimensionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type storativity(storativitySEXP);
    Rcpp::traits::input_parameter< double >::type transmissivity(transmissivitySEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type time(timeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type flow_rate(flow_rateSEXP);
    Rcpp::traits::input_parameter< int >::type flow_time_interval(flow_time_intervalSEXP);
    Rcpp::traits::input_parameter< double >::type flow_dimension(flow_dimensionSEXP);
    rcpp_result_gen = Rcpp::wrap(grf_time_parallel(radius, storativity, transmissivity, time, flow_rate, flow_time_interval, flow_dimension));
    return rcpp_result_gen;
END_RCPP
}
// hantush_time_parallel
Rcpp::NumericVector hantush_time_parallel(double radius, double storativity, double transmissivity, double leakage, const Rcpp::NumericVector& time, const Rcpp::NumericVector& flow_rate, int flow_time_interval, int n_terms);
RcppExport SEXP aquifer_hantush_time_parallel(SEXP radiusSEXP, SEXP storativitySEXP, SEXP transmissivitySEXP, SEXP leakageSEXP, SEXP timeSEXP, SEXP flow_rateSEXP, SEXP flow_time_intervalSEXP, SEXP n_termsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type storativity(storativitySEXP);
    Rcpp::traits::input_parameter< double >::type transmissivity(transmissivitySEXP);
    Rcpp::traits::input_parameter< double >::type leakage(leakageSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type time(timeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type flow_rate(flow_rateSEXP);
    Rcpp::traits::input_parameter< int >::type flow_time_interval(flow_time_intervalSEXP);
    Rcpp::traits::input_parameter< int >::type n_terms(n_termsSEXP);
    rcpp_result_gen = Rcpp::wrap(hantush_time_parallel(radius, storativity, transmissivity, leakage, time, flow_rate, flow_time_interval, n_terms));
    return rcpp_result_gen;
END_RCPP
}
