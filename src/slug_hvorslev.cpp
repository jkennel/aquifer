#include "aquifer.h"

// H, Lw, Li, Rw, Rc, m, solid_bottom, bounded

// [[Rcpp::export]]
double hvorslev_shape(double x, double d, double r, double l, int case_num) {

  double shape_f;
  // L < r
  // L > 0
  // Mathias and Butler, 2006
  if (case_num == 7) {

    if(l == 0) {
      shape_f = 4 * r;
    } else if (l == r) {
      shape_f = 2* M_PI * r;
    } else if (l / r > 1) {
      shape_f = (2 * M_PI * l) /
        std::log(2 * l / r);
    } else {

      shape_f = (2 * M_PI * r) /
        (std::sinh(std::atanh(r/l)) *
        std::log(1.0/std::tanh(0.5*std::atanh(r/l))));
    }

  }

  // L < D
  // L > 0
  // Mathias and Butler, 2006
  if (case_num == 8) {

    if(l == 0) {
      shape_f = 4 * d;
    } else if (l == d) {
      shape_f = 2 * M_PI * d;
    } else {
      shape_f = (2 * M_PI * d) /
        (std::sinh(std::atanh(d / l)) *
        std::log(1.0 / std::tanh(0.5 * std::atanh(d / l))));
    }
  }

  return(shape_f);


}



/*** R

# double x, double d, double r, double l, int case_num
hvorslev_shape(1, 0.1, 1.0, 2.0, 8)
*/
