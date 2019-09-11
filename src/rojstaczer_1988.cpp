#include "aquifer.h"


//' @title
//' rojstaczer
//'
//' @description
//' Modified Bessel function of second kind order 0
//'
//' @param frequency
//' @param radius_well
//' @param transmissivity
//' @param storage_confining
//' @param storage_aquifer
//' @param diffusivity_confining
//' @param diffusivity_vadose
//' @param thickness_confining
//' @param thickness_vadose
//' @param be
//' @param attenuations
//'
//' @return bessel function result
//'
//'
//' @export
// [[Rcpp::export]]
arma::cx_double rojstaczer(double frequency,
                           double radius_well,
                           double transmissivity,
                           double storage_confining,
                           double storage_aquifer,
                           double diffusivity_confining,
                           double diffusivity_vadose,
                           double thickness_confining,
                           double thickness_vadose,
                           double loading_efficiency,
                           double attenuation,
                           arma::vec gamma_term_a,
                           arma::vec gamma_term_b) {

  arma::cx_double ii = arma::cx_double(0.0, 1.0);

  double omega = (2.0 * M_PI * frequency);
  double W = (radius_well * radius_well * omega) / transmissivity;
  double Q = (thickness_confining * thickness_confining * omega) / (2.0 * diffusivity_confining); // eq 10
  double SQR = sqrt((thickness_vadose * thickness_vadose) * omega / (2.0 * diffusivity_vadose)); // eq 5

  // eq 4a, 4b
  double MN_denom = (cosh(2.0 * (SQR)) + cos(2.0 * (SQR))) / (2.0*attenuation);
  double M = (cosh(SQR) * cos(SQR)) / MN_denom;
  double N = (sinh(SQR) * sin(SQR)) / MN_denom;
  arma::cx_double MN = arma::cx_double(M, N);


  arma::cx_double p0 = ((MN - (loading_efficiency)) * std::exp(-(ii + 1.0) * (sqrt(Q))) + (loading_efficiency));


  arma::cx_double k0 = bessel_k_complex_opt(
    std::pow((W * W) * ((storage_aquifer * storage_aquifer) +
      pow(storage_confining / (2.0 * Q), 2)), 0.25) *
      std::exp(0.5 * ii * (atan(2.0 * Q))),
      gamma_term_a, gamma_term_b);



  arma::cx_double x1 = (-1.0 + p0) / (1.0 + (ii * 0.5 * W * k0));

  return(x1);


}



//==============================================================================
// Parallel version of rojstaczer 1988
// The bessel function for complex arguments is currently not optimized.
// Bessel::BesselK is more efficient in non-parallel versions
//==============================================================================
struct rojstaczer_worker : public Worker
{

  const arma::vec frequency;
  const double radius_well;
  const double transmissivity;
  const double storage_confining;
  const double storage_aquifer;
  const double diffusivity_confining;
  const double diffusivity_vadose;
  const double thickness_confining;
  const double thickness_vadose;
  const double loading_efficiency;
  const double attenuation;
  const arma::vec gamma_term_a;
  const arma::vec gamma_term_b;

  arma::cx_vec& output;


  rojstaczer_worker(const arma::vec frequency,
                    const double radius_well,
                    const double transmissivity,
                    const double storage_confining,
                    const double storage_aquifer,
                    const double diffusivity_confining,
                    const double diffusivity_vadose,
                    const double thickness_confining,
                    const double thickness_vadose,
                    const double loading_efficiency,
                    const double attenuation,
                    const arma::vec gamma_term_a,
                    const arma::vec gamma_term_b,
                    arma::cx_vec& output)
    : frequency(frequency), radius_well(radius_well), transmissivity(transmissivity),
      storage_confining(storage_confining), storage_aquifer(storage_aquifer),
      diffusivity_confining(diffusivity_confining), diffusivity_vadose(diffusivity_vadose),
      thickness_confining(thickness_confining), thickness_vadose(thickness_vadose),
      loading_efficiency(loading_efficiency), attenuation(attenuation),
      gamma_term_a(gamma_term_a), gamma_term_b(gamma_term_b), output(output) {}

  // calculate the exponential integral
  void operator()(std::size_t begin_row, std::size_t end_row) {
    for (std::size_t i = begin_row; i < end_row; i++) {
      output[i] = rojstaczer(frequency[i],
                          radius_well,
                          transmissivity,
                          storage_confining,
                          storage_aquifer,
                          diffusivity_confining,
                          diffusivity_vadose,
                          thickness_confining,
                          thickness_vadose,
                          loading_efficiency,
                          attenuation,
                          gamma_term_a,
                          gamma_term_b);
    }
  }


};

//' @title
//' rojstaczer_parallel
//'
//' @description
//' Modified Bessel function of second kind order 0
//'
//' @param frequency
//' @param radius_well
//' @param transmissivity
//' @param storage_confining
//' @param storage_aquifer
//' @param diffusivity_confining
//' @param diffusivity_vadose
//' @param thickness_confining
//' @param thickness_vadose
//' @param loading_efficiency
//' @param attenuations
//' @param gamma_term_a
//' @param gamma_term_b
//'
//' @return bessel function result
//'
//'
//' @export
// [[Rcpp::export]]
arma::cx_vec rojstaczer_parallel(const arma::vec frequency,
                                 double radius_well,
                                 double transmissivity,
                                 double storage_confining,
                                 double storage_aquifer,
                                 double diffusivity_confining,
                                 double diffusivity_vadose,
                                 double thickness_confining,
                                 double thickness_vadose,
                                 double loading_efficiency,
                                 double attenuation) {

  int n = frequency.n_elem;
  arma::cx_vec output(n);
  output.ones();

  int n_terms = 13;
  arma::vec gamma_term_a(n_terms);
  arma::vec gamma_term_b(n_terms);
  double v = 1e-8;

  for (int j = 0; j < n_terms; j++) {
    gamma_term_a(j) = bessel_i_gamma_term(j, v);
    gamma_term_b(j) = bessel_i_gamma_term(j, -v);
  }

  rojstaczer_worker befr(frequency,
                          radius_well,
                          transmissivity,
                          storage_confining,
                          storage_aquifer,
                          diffusivity_confining,
                          diffusivity_vadose,
                          thickness_confining,
                          thickness_vadose,
                          loading_efficiency,
                          attenuation,
                          gamma_term_a,
                          gamma_term_b,
                          output);

  RcppParallel::parallelFor(0, n, befr);

  return(output);

}

/*** R

*/
