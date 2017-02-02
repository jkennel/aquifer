#include "aquifer.h"

//==============================================================================
//' @title
//' well_function_coefficient
//'
//' @description
//' Calculation Coefficient Q/(4 pi * T)
//'
//' @param flow_rate well flow rates
//' @param transmissivity aquifer transmissivity
//'
//' @return coefficient for Theis and Hantush well function
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericVector well_function_coefficient(Rcpp::NumericVector flow_rate, double transmissivity){

  return( flow_rate / (4 * PI * transmissivity));

}

//==============================================================================
//' @title
//' hantush_epsilon
//'
//' @description
//' Calculation of r^2/(4B^2)
//'
//' @param radius distance to monitoring well
//' @param leakage aquifer transmissivity
//'
//' @return coefficient Hantush well function
//'
//'
//' @export
//'
// [[Rcpp::export]]
double hantush_epsilon(double radius, double leakage){

  return((radius*radius) / (4*leakage*leakage));

}

//==============================================================================
//' @title
//' grf_coefficient
//'
//' @description
//' Coefficient for the grf without pumping
//'
//' @param flow_rate (vector) the flow rate
//' @param radius (double) distance to center of source
//' @param K (double) hydraulic conductivity of the fracture system
//' @param thickness (double) thickness
//' @param flow_dimension (double) flow dimension
//'
//' @return coefficient
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericVector grf_coefficient(Rcpp::NumericVector flow_rate,
                                    double radius,
                                    double K,
                                    double thickness,
                                    double flow_dimension){

  double v = 1 - flow_dimension/2;

  if(flow_dimension == 2){

    return well_function_coefficient(flow_rate, K * thickness);

  } else {

    return( flow_rate * pow(radius, (2 * v)) /
            (4 * pow(PI, (1 - v)) * K * pow(thickness, (3 - flow_dimension))));

  }
}

//==============================================================================
//' @title
//' theis_u_time
//'
//' @description
//' Calculation of Theis u
//'
//' @param radius distance to monitoring interval
//' @param storativity aquifer storativity
//' @param transmissivity aquifer transmissivity
//' @param time prediction times
//'
//' @return u for well function
//'
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericVector theis_u_time(double radius,
                           double storativity,
                           double transmissivity,
                           const Rcpp::NumericVector& time) {

  Rcpp::NumericVector out = (radius * radius * storativity) /
    (4.0 * transmissivity * time);

  return out;
}


// //==============================================================================
// //' @title
// //' theis_u_radius
// //'
// //' @description
// //' Calculation of Theis u
// //'
// //' @param radius distance to monitoring interval
// //' @param storativity aquifer storativity
// //' @param transmissivity aquifer transmissivity
// //' @param time prediction times
// //'
// //' @return u for well function
// //'
// //'
// //' @export
// //'
// // [[Rcpp::export]]
// Rcpp::NumericVector theis_u_radius(const Rcpp::NumericVector& radius,
//                              double storativity,
//                              double transmissivity,
//                              double  time) {
//
//   Rcpp::NumericVector out = (radius * radius * storativity) /
//     (4.0 * transmissivity * time);
//
//   return out;
// }
