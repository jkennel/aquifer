// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// impulse_function
Rcpp::NumericVector impulse_function(Rcpp::NumericVector& u, double flow_time_interval);
RcppExport SEXP _aquifer_impulse_function(SEXP uSEXP, SEXP flow_time_intervalSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type flow_time_interval(flow_time_intervalSEXP);
    rcpp_result_gen = Rcpp::wrap(impulse_function(u, flow_time_interval));
    return rcpp_result_gen;
END_RCPP
}
// ogata_banks_ind
double ogata_banks_ind(double D, double v, double C0, double x, double t);
RcppExport SEXP _aquifer_ogata_banks_ind(SEXP DSEXP, SEXP vSEXP, SEXP C0SEXP, SEXP xSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type D(DSEXP);
    Rcpp::traits::input_parameter< double >::type v(vSEXP);
    Rcpp::traits::input_parameter< double >::type C0(C0SEXP);
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(ogata_banks_ind(D, v, C0, x, t));
    return rcpp_result_gen;
END_RCPP
}
// ogata_banks
arma::mat ogata_banks(double D, double R, double decay, double v, double C0, arma::vec x, arma::rowvec t);
RcppExport SEXP _aquifer_ogata_banks(SEXP DSEXP, SEXP RSEXP, SEXP decaySEXP, SEXP vSEXP, SEXP C0SEXP, SEXP xSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type D(DSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    Rcpp::traits::input_parameter< double >::type decay(decaySEXP);
    Rcpp::traits::input_parameter< double >::type v(vSEXP);
    Rcpp::traits::input_parameter< double >::type C0(C0SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::rowvec >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(ogata_banks(D, R, decay, v, C0, x, t));
    return rcpp_result_gen;
END_RCPP
}
// rojstaczer
arma::cx_double rojstaczer(double frequency, double radius_well, double transmissivity, double storage_confining, double storage_aquifer, double diffusivity_confining, double diffusivity_vadose, double thickness_confining, double thickness_vadose, double loading_efficiency, double attenuation, arma::vec gamma_term_a, arma::vec gamma_term_b);
RcppExport SEXP _aquifer_rojstaczer(SEXP frequencySEXP, SEXP radius_wellSEXP, SEXP transmissivitySEXP, SEXP storage_confiningSEXP, SEXP storage_aquiferSEXP, SEXP diffusivity_confiningSEXP, SEXP diffusivity_vadoseSEXP, SEXP thickness_confiningSEXP, SEXP thickness_vadoseSEXP, SEXP loading_efficiencySEXP, SEXP attenuationSEXP, SEXP gamma_term_aSEXP, SEXP gamma_term_bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type frequency(frequencySEXP);
    Rcpp::traits::input_parameter< double >::type radius_well(radius_wellSEXP);
    Rcpp::traits::input_parameter< double >::type transmissivity(transmissivitySEXP);
    Rcpp::traits::input_parameter< double >::type storage_confining(storage_confiningSEXP);
    Rcpp::traits::input_parameter< double >::type storage_aquifer(storage_aquiferSEXP);
    Rcpp::traits::input_parameter< double >::type diffusivity_confining(diffusivity_confiningSEXP);
    Rcpp::traits::input_parameter< double >::type diffusivity_vadose(diffusivity_vadoseSEXP);
    Rcpp::traits::input_parameter< double >::type thickness_confining(thickness_confiningSEXP);
    Rcpp::traits::input_parameter< double >::type thickness_vadose(thickness_vadoseSEXP);
    Rcpp::traits::input_parameter< double >::type loading_efficiency(loading_efficiencySEXP);
    Rcpp::traits::input_parameter< double >::type attenuation(attenuationSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type gamma_term_a(gamma_term_aSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type gamma_term_b(gamma_term_bSEXP);
    rcpp_result_gen = Rcpp::wrap(rojstaczer(frequency, radius_well, transmissivity, storage_confining, storage_aquifer, diffusivity_confining, diffusivity_vadose, thickness_confining, thickness_vadose, loading_efficiency, attenuation, gamma_term_a, gamma_term_b));
    return rcpp_result_gen;
END_RCPP
}
// rojstaczer_parallel
arma::cx_vec rojstaczer_parallel(const arma::vec frequency, double radius_well, double transmissivity, double storage_confining, double storage_aquifer, double diffusivity_confining, double diffusivity_vadose, double thickness_confining, double thickness_vadose, double loading_efficiency, double attenuation);
RcppExport SEXP _aquifer_rojstaczer_parallel(SEXP frequencySEXP, SEXP radius_wellSEXP, SEXP transmissivitySEXP, SEXP storage_confiningSEXP, SEXP storage_aquiferSEXP, SEXP diffusivity_confiningSEXP, SEXP diffusivity_vadoseSEXP, SEXP thickness_confiningSEXP, SEXP thickness_vadoseSEXP, SEXP loading_efficiencySEXP, SEXP attenuationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec >::type frequency(frequencySEXP);
    Rcpp::traits::input_parameter< double >::type radius_well(radius_wellSEXP);
    Rcpp::traits::input_parameter< double >::type transmissivity(transmissivitySEXP);
    Rcpp::traits::input_parameter< double >::type storage_confining(storage_confiningSEXP);
    Rcpp::traits::input_parameter< double >::type storage_aquifer(storage_aquiferSEXP);
    Rcpp::traits::input_parameter< double >::type diffusivity_confining(diffusivity_confiningSEXP);
    Rcpp::traits::input_parameter< double >::type diffusivity_vadose(diffusivity_vadoseSEXP);
    Rcpp::traits::input_parameter< double >::type thickness_confining(thickness_confiningSEXP);
    Rcpp::traits::input_parameter< double >::type thickness_vadose(thickness_vadoseSEXP);
    Rcpp::traits::input_parameter< double >::type loading_efficiency(loading_efficiencySEXP);
    Rcpp::traits::input_parameter< double >::type attenuation(attenuationSEXP);
    rcpp_result_gen = Rcpp::wrap(rojstaczer_parallel(frequency, radius_well, transmissivity, storage_confining, storage_aquifer, diffusivity_confining, diffusivity_vadose, thickness_confining, thickness_vadose, loading_efficiency, attenuation));
    return rcpp_result_gen;
END_RCPP
}
// a_cpp
double a_cpp(double x);
RcppExport SEXP _aquifer_a_cpp(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(a_cpp(x));
    return rcpp_result_gen;
END_RCPP
}
// b_cpp
double b_cpp(double x);
RcppExport SEXP _aquifer_b_cpp(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(b_cpp(x));
    return rcpp_result_gen;
END_RCPP
}
// c_cpp
double c_cpp(double x);
RcppExport SEXP _aquifer_c_cpp(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(c_cpp(x));
    return rcpp_result_gen;
END_RCPP
}
// bouwer_rice_abc
double bouwer_rice_abc(double rw, double Le, double Lw, double H);
RcppExport SEXP _aquifer_bouwer_rice_abc(SEXP rwSEXP, SEXP LeSEXP, SEXP LwSEXP, SEXP HSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type rw(rwSEXP);
    Rcpp::traits::input_parameter< double >::type Le(LeSEXP);
    Rcpp::traits::input_parameter< double >::type Lw(LwSEXP);
    Rcpp::traits::input_parameter< double >::type H(HSEXP);
    rcpp_result_gen = Rcpp::wrap(bouwer_rice_abc(rw, Le, Lw, H));
    return rcpp_result_gen;
END_RCPP
}
// bouwer_rice
double bouwer_rice(arma::vec time, arma::vec drawdown, double radius_screen, double radius_casing, double Le, double Lw, double H);
RcppExport SEXP _aquifer_bouwer_rice(SEXP timeSEXP, SEXP drawdownSEXP, SEXP radius_screenSEXP, SEXP radius_casingSEXP, SEXP LeSEXP, SEXP LwSEXP, SEXP HSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type time(timeSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type drawdown(drawdownSEXP);
    Rcpp::traits::input_parameter< double >::type radius_screen(radius_screenSEXP);
    Rcpp::traits::input_parameter< double >::type radius_casing(radius_casingSEXP);
    Rcpp::traits::input_parameter< double >::type Le(LeSEXP);
    Rcpp::traits::input_parameter< double >::type Lw(LwSEXP);
    Rcpp::traits::input_parameter< double >::type H(HSEXP);
    rcpp_result_gen = Rcpp::wrap(bouwer_rice(time, drawdown, radius_screen, radius_casing, Le, Lw, H));
    return rcpp_result_gen;
END_RCPP
}
// slug_cbp_laplace_single
double slug_cbp_laplace_single(double p, double radius_casing, double radius_screen, double radius, double storativity, double transmissivity, double head_0);
RcppExport SEXP _aquifer_slug_cbp_laplace_single(SEXP pSEXP, SEXP radius_casingSEXP, SEXP radius_screenSEXP, SEXP radiusSEXP, SEXP storativitySEXP, SEXP transmissivitySEXP, SEXP head_0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type radius_casing(radius_casingSEXP);
    Rcpp::traits::input_parameter< double >::type radius_screen(radius_screenSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type storativity(storativitySEXP);
    Rcpp::traits::input_parameter< double >::type transmissivity(transmissivitySEXP);
    Rcpp::traits::input_parameter< double >::type head_0(head_0SEXP);
    rcpp_result_gen = Rcpp::wrap(slug_cbp_laplace_single(p, radius_casing, radius_screen, radius, storativity, transmissivity, head_0));
    return rcpp_result_gen;
END_RCPP
}
// slug_cbp_parallel
Rcpp::NumericMatrix slug_cbp_parallel(Rcpp::NumericMatrix p, double radius_casing, double radius_screen, double radius, double storativity, double transmissivity, double head_0);
RcppExport SEXP _aquifer_slug_cbp_parallel(SEXP pSEXP, SEXP radius_casingSEXP, SEXP radius_screenSEXP, SEXP radiusSEXP, SEXP storativitySEXP, SEXP transmissivitySEXP, SEXP head_0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type radius_casing(radius_casingSEXP);
    Rcpp::traits::input_parameter< double >::type radius_screen(radius_screenSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type storativity(storativitySEXP);
    Rcpp::traits::input_parameter< double >::type transmissivity(transmissivitySEXP);
    Rcpp::traits::input_parameter< double >::type head_0(head_0SEXP);
    rcpp_result_gen = Rcpp::wrap(slug_cbp_parallel(p, radius_casing, radius_screen, radius, storativity, transmissivity, head_0));
    return rcpp_result_gen;
END_RCPP
}
// hvorslev_shape
double hvorslev_shape(double x, double d, double r, double l, int case_num);
RcppExport SEXP _aquifer_hvorslev_shape(SEXP xSEXP, SEXP dSEXP, SEXP rSEXP, SEXP lSEXP, SEXP case_numSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type d(dSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type l(lSEXP);
    Rcpp::traits::input_parameter< int >::type case_num(case_numSEXP);
    rcpp_result_gen = Rcpp::wrap(hvorslev_shape(x, d, r, l, case_num));
    return rcpp_result_gen;
END_RCPP
}
// bessel_k_single
double bessel_k_single(double x, int nu);
RcppExport SEXP _aquifer_bessel_k_single(SEXP xSEXP, SEXP nuSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nu(nuSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_k_single(x, nu));
    return rcpp_result_gen;
END_RCPP
}
// bessel_i_gamma_term
double bessel_i_gamma_term(double jj, double v);
RcppExport SEXP _aquifer_bessel_i_gamma_term(SEXP jjSEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type jj(jjSEXP);
    Rcpp::traits::input_parameter< double >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_i_gamma_term(jj, v));
    return rcpp_result_gen;
END_RCPP
}
// bessel_i_complex_new
arma::cx_double bessel_i_complex_new(arma::cx_double x, double v, arma::vec gamma_term);
RcppExport SEXP _aquifer_bessel_i_complex_new(SEXP xSEXP, SEXP vSEXP, SEXP gamma_termSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cx_double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type v(vSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type gamma_term(gamma_termSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_i_complex_new(x, v, gamma_term));
    return rcpp_result_gen;
END_RCPP
}
// bessel_k_complex_opt
arma::cx_double bessel_k_complex_opt(const arma::cx_double x, arma::vec gamma_term_a, arma::vec gamma_term_b);
RcppExport SEXP _aquifer_bessel_k_complex_opt(SEXP xSEXP, SEXP gamma_term_aSEXP, SEXP gamma_term_bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::cx_double >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type gamma_term_a(gamma_term_aSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type gamma_term_b(gamma_term_bSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_k_complex_opt(x, gamma_term_a, gamma_term_b));
    return rcpp_result_gen;
END_RCPP
}
// bessel_i_complex
arma::cx_double bessel_i_complex(arma::cx_double x, double v);
RcppExport SEXP _aquifer_bessel_i_complex(SEXP xSEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cx_double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_i_complex(x, v));
    return rcpp_result_gen;
END_RCPP
}
// bessel_k_complex
arma::cx_rowvec bessel_k_complex(const arma::cx_rowvec& x);
RcppExport SEXP _aquifer_bessel_k_complex(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::cx_rowvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_k_complex(x));
    return rcpp_result_gen;
END_RCPP
}
// bessel_k_complex_single
arma::cx_double bessel_k_complex_single(arma::cx_double x);
RcppExport SEXP _aquifer_bessel_k_complex_single(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cx_double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_k_complex_single(x));
    return rcpp_result_gen;
END_RCPP
}
// bessel_k_parallel
Rcpp::NumericMatrix bessel_k_parallel(Rcpp::NumericMatrix x, int nu);
RcppExport SEXP _aquifer_bessel_k_parallel(SEXP xSEXP, SEXP nuSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nu(nuSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_k_parallel(x, nu));
    return rcpp_result_gen;
END_RCPP
}
// bessel_k_complex_parallel
arma::cx_vec bessel_k_complex_parallel(arma::cx_vec x);
RcppExport SEXP _aquifer_bessel_k_complex_parallel(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cx_vec >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_k_complex_parallel(x));
    return rcpp_result_gen;
END_RCPP
}
// bh_tgamma
double bh_tgamma(double u, double a);
RcppExport SEXP _aquifer_bh_tgamma(SEXP uSEXP, SEXP aSEXP) {
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
RcppExport SEXP _aquifer_bh_gamma_neg(SEXP uSEXP, SEXP aSEXP) {
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
RcppExport SEXP _aquifer_gamma_der(SEXP uSEXP, SEXP aSEXP) {
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
RcppExport SEXP _aquifer_exp_int_single(SEXP uSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(exp_int_single(u, a));
    return rcpp_result_gen;
END_RCPP
}
// grf_parallel
Rcpp::NumericVector grf_parallel(Rcpp::NumericVector u, double a);
RcppExport SEXP _aquifer_grf_parallel(SEXP uSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(grf_parallel(u, a));
    return rcpp_result_gen;
END_RCPP
}
// hantush_well_single
double hantush_well_single(double u, double b, int n_terms);
RcppExport SEXP _aquifer_hantush_well_single(SEXP uSEXP, SEXP bSEXP, SEXP n_termsSEXP) {
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
// hantush_well_parallel
Rcpp::NumericVector hantush_well_parallel(Rcpp::NumericVector u, double b, int n_terms);
RcppExport SEXP _aquifer_hantush_well_parallel(SEXP uSEXP, SEXP bSEXP, SEXP n_termsSEXP) {
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
// weeks_1979
double weeks_1979(double lag, double D, double L, double precision, bool inverse);
RcppExport SEXP _aquifer_weeks_1979(SEXP lagSEXP, SEXP DSEXP, SEXP LSEXP, SEXP precisionSEXP, SEXP inverseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type lag(lagSEXP);
    Rcpp::traits::input_parameter< double >::type D(DSEXP);
    Rcpp::traits::input_parameter< double >::type L(LSEXP);
    Rcpp::traits::input_parameter< double >::type precision(precisionSEXP);
    Rcpp::traits::input_parameter< bool >::type inverse(inverseSEXP);
    rcpp_result_gen = Rcpp::wrap(weeks_1979(lag, D, L, precision, inverse));
    return rcpp_result_gen;
END_RCPP
}
// vadose_response
Rcpp::NumericVector vadose_response(const Rcpp::NumericVector time, double D, double L, double precision, bool inverse);
RcppExport SEXP _aquifer_vadose_response(SEXP timeSEXP, SEXP DSEXP, SEXP LSEXP, SEXP precisionSEXP, SEXP inverseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector >::type time(timeSEXP);
    Rcpp::traits::input_parameter< double >::type D(DSEXP);
    Rcpp::traits::input_parameter< double >::type L(LSEXP);
    Rcpp::traits::input_parameter< double >::type precision(precisionSEXP);
    Rcpp::traits::input_parameter< bool >::type inverse(inverseSEXP);
    rcpp_result_gen = Rcpp::wrap(vadose_response(time, D, L, precision, inverse));
    return rcpp_result_gen;
END_RCPP
}
// well_function_coefficient
Rcpp::NumericVector well_function_coefficient(Rcpp::NumericVector flow_rate, double transmissivity);
RcppExport SEXP _aquifer_well_function_coefficient(SEXP flow_rateSEXP, SEXP transmissivitySEXP) {
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
RcppExport SEXP _aquifer_hantush_epsilon(SEXP radiusSEXP, SEXP leakageSEXP) {
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
RcppExport SEXP _aquifer_grf_coefficient(SEXP flow_rateSEXP, SEXP radiusSEXP, SEXP KSEXP, SEXP thicknessSEXP, SEXP flow_dimensionSEXP) {
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
RcppExport SEXP _aquifer_theis_u_time(SEXP radiusSEXP, SEXP storativitySEXP, SEXP transmissivitySEXP, SEXP timeSEXP) {
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
// grf_u_time
Rcpp::NumericVector grf_u_time(double radius, double storativity, double K, const Rcpp::NumericVector& time);
RcppExport SEXP _aquifer_grf_u_time(SEXP radiusSEXP, SEXP storativitySEXP, SEXP KSEXP, SEXP timeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type storativity(storativitySEXP);
    Rcpp::traits::input_parameter< double >::type K(KSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type time(timeSEXP);
    rcpp_result_gen = Rcpp::wrap(grf_u_time(radius, storativity, K, time));
    return rcpp_result_gen;
END_RCPP
}
// well_function_convolve
Rcpp::NumericVector well_function_convolve(int flow_time_interval, const Rcpp::NumericVector& coefs, const Rcpp::NumericVector& u);
RcppExport SEXP _aquifer_well_function_convolve(SEXP flow_time_intervalSEXP, SEXP coefsSEXP, SEXP uSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type flow_time_interval(flow_time_intervalSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type coefs(coefsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type u(uSEXP);
    rcpp_result_gen = Rcpp::wrap(well_function_convolve(flow_time_interval, coefs, u));
    return rcpp_result_gen;
END_RCPP
}
// grf_time_parallel
Rcpp::NumericVector grf_time_parallel(double radius, double storativity, double K, double thickness, const Rcpp::NumericVector& time, const Rcpp::NumericVector& flow_rate, int flow_time_interval, double flow_dimension);
RcppExport SEXP _aquifer_grf_time_parallel(SEXP radiusSEXP, SEXP storativitySEXP, SEXP KSEXP, SEXP thicknessSEXP, SEXP timeSEXP, SEXP flow_rateSEXP, SEXP flow_time_intervalSEXP, SEXP flow_dimensionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type storativity(storativitySEXP);
    Rcpp::traits::input_parameter< double >::type K(KSEXP);
    Rcpp::traits::input_parameter< double >::type thickness(thicknessSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type time(timeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type flow_rate(flow_rateSEXP);
    Rcpp::traits::input_parameter< int >::type flow_time_interval(flow_time_intervalSEXP);
    Rcpp::traits::input_parameter< double >::type flow_dimension(flow_dimensionSEXP);
    rcpp_result_gen = Rcpp::wrap(grf_time_parallel(radius, storativity, K, thickness, time, flow_rate, flow_time_interval, flow_dimension));
    return rcpp_result_gen;
END_RCPP
}
// hantush_time_parallel
Rcpp::NumericVector hantush_time_parallel(double radius, double storativity, double transmissivity, double leakage, const Rcpp::NumericVector& time, const Rcpp::NumericVector& flow_rate, int flow_time_interval, int n_terms);
RcppExport SEXP _aquifer_hantush_time_parallel(SEXP radiusSEXP, SEXP storativitySEXP, SEXP transmissivitySEXP, SEXP leakageSEXP, SEXP timeSEXP, SEXP flow_rateSEXP, SEXP flow_time_intervalSEXP, SEXP n_termsSEXP) {
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
// hantush
Rcpp::NumericVector hantush(double radius, double storativity, double transmissivity, double leakage, const Rcpp::NumericVector& time, const Rcpp::NumericVector& flow_rate, const Rcpp::NumericVector& flow_rate_times, int n_terms);
RcppExport SEXP _aquifer_hantush(SEXP radiusSEXP, SEXP storativitySEXP, SEXP transmissivitySEXP, SEXP leakageSEXP, SEXP timeSEXP, SEXP flow_rateSEXP, SEXP flow_rate_timesSEXP, SEXP n_termsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type storativity(storativitySEXP);
    Rcpp::traits::input_parameter< double >::type transmissivity(transmissivitySEXP);
    Rcpp::traits::input_parameter< double >::type leakage(leakageSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type time(timeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type flow_rate(flow_rateSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type flow_rate_times(flow_rate_timesSEXP);
    Rcpp::traits::input_parameter< int >::type n_terms(n_termsSEXP);
    rcpp_result_gen = Rcpp::wrap(hantush(radius, storativity, transmissivity, leakage, time, flow_rate, flow_rate_times, n_terms));
    return rcpp_result_gen;
END_RCPP
}
// grf
Rcpp::NumericVector grf(double radius, double storativity, double K, double thickness, const Rcpp::NumericVector& time, const Rcpp::NumericVector& flow_rate, const Rcpp::NumericVector& flow_rate_times, double flow_dimension);
RcppExport SEXP _aquifer_grf(SEXP radiusSEXP, SEXP storativitySEXP, SEXP KSEXP, SEXP thicknessSEXP, SEXP timeSEXP, SEXP flow_rateSEXP, SEXP flow_rate_timesSEXP, SEXP flow_dimensionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type storativity(storativitySEXP);
    Rcpp::traits::input_parameter< double >::type K(KSEXP);
    Rcpp::traits::input_parameter< double >::type thickness(thicknessSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type time(timeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type flow_rate(flow_rateSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type flow_rate_times(flow_rate_timesSEXP);
    Rcpp::traits::input_parameter< double >::type flow_dimension(flow_dimensionSEXP);
    rcpp_result_gen = Rcpp::wrap(grf(radius, storativity, K, thickness, time, flow_rate, flow_rate_times, flow_dimension));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_aquifer_impulse_function", (DL_FUNC) &_aquifer_impulse_function, 2},
    {"_aquifer_ogata_banks_ind", (DL_FUNC) &_aquifer_ogata_banks_ind, 5},
    {"_aquifer_ogata_banks", (DL_FUNC) &_aquifer_ogata_banks, 7},
    {"_aquifer_rojstaczer", (DL_FUNC) &_aquifer_rojstaczer, 13},
    {"_aquifer_rojstaczer_parallel", (DL_FUNC) &_aquifer_rojstaczer_parallel, 11},
    {"_aquifer_a_cpp", (DL_FUNC) &_aquifer_a_cpp, 1},
    {"_aquifer_b_cpp", (DL_FUNC) &_aquifer_b_cpp, 1},
    {"_aquifer_c_cpp", (DL_FUNC) &_aquifer_c_cpp, 1},
    {"_aquifer_bouwer_rice_abc", (DL_FUNC) &_aquifer_bouwer_rice_abc, 4},
    {"_aquifer_bouwer_rice", (DL_FUNC) &_aquifer_bouwer_rice, 7},
    {"_aquifer_slug_cbp_laplace_single", (DL_FUNC) &_aquifer_slug_cbp_laplace_single, 7},
    {"_aquifer_slug_cbp_parallel", (DL_FUNC) &_aquifer_slug_cbp_parallel, 7},
    {"_aquifer_hvorslev_shape", (DL_FUNC) &_aquifer_hvorslev_shape, 5},
    {"_aquifer_bessel_k_single", (DL_FUNC) &_aquifer_bessel_k_single, 2},
    {"_aquifer_bessel_i_gamma_term", (DL_FUNC) &_aquifer_bessel_i_gamma_term, 2},
    {"_aquifer_bessel_i_complex_new", (DL_FUNC) &_aquifer_bessel_i_complex_new, 3},
    {"_aquifer_bessel_k_complex_opt", (DL_FUNC) &_aquifer_bessel_k_complex_opt, 3},
    {"_aquifer_bessel_i_complex", (DL_FUNC) &_aquifer_bessel_i_complex, 2},
    {"_aquifer_bessel_k_complex", (DL_FUNC) &_aquifer_bessel_k_complex, 1},
    {"_aquifer_bessel_k_complex_single", (DL_FUNC) &_aquifer_bessel_k_complex_single, 1},
    {"_aquifer_bessel_k_parallel", (DL_FUNC) &_aquifer_bessel_k_parallel, 2},
    {"_aquifer_bessel_k_complex_parallel", (DL_FUNC) &_aquifer_bessel_k_complex_parallel, 1},
    {"_aquifer_bh_tgamma", (DL_FUNC) &_aquifer_bh_tgamma, 2},
    {"_aquifer_bh_gamma_neg", (DL_FUNC) &_aquifer_bh_gamma_neg, 2},
    {"_aquifer_gamma_der", (DL_FUNC) &_aquifer_gamma_der, 2},
    {"_aquifer_exp_int_single", (DL_FUNC) &_aquifer_exp_int_single, 2},
    {"_aquifer_grf_parallel", (DL_FUNC) &_aquifer_grf_parallel, 2},
    {"_aquifer_hantush_well_single", (DL_FUNC) &_aquifer_hantush_well_single, 3},
    {"_aquifer_hantush_well_parallel", (DL_FUNC) &_aquifer_hantush_well_parallel, 3},
    {"_aquifer_weeks_1979", (DL_FUNC) &_aquifer_weeks_1979, 5},
    {"_aquifer_vadose_response", (DL_FUNC) &_aquifer_vadose_response, 5},
    {"_aquifer_well_function_coefficient", (DL_FUNC) &_aquifer_well_function_coefficient, 2},
    {"_aquifer_hantush_epsilon", (DL_FUNC) &_aquifer_hantush_epsilon, 2},
    {"_aquifer_grf_coefficient", (DL_FUNC) &_aquifer_grf_coefficient, 5},
    {"_aquifer_theis_u_time", (DL_FUNC) &_aquifer_theis_u_time, 4},
    {"_aquifer_grf_u_time", (DL_FUNC) &_aquifer_grf_u_time, 4},
    {"_aquifer_well_function_convolve", (DL_FUNC) &_aquifer_well_function_convolve, 3},
    {"_aquifer_grf_time_parallel", (DL_FUNC) &_aquifer_grf_time_parallel, 8},
    {"_aquifer_hantush_time_parallel", (DL_FUNC) &_aquifer_hantush_time_parallel, 8},
    {"_aquifer_hantush", (DL_FUNC) &_aquifer_hantush, 8},
    {"_aquifer_grf", (DL_FUNC) &_aquifer_grf, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_aquifer(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
