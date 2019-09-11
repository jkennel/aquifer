#' hsieh_1987
#' Solution for estimating transmissivity and storativity from earth tides.
#'
#' @param frequency
#' @param storativity
#' @param transmissivity
#' @param radius_well
#'
#' @return
#' @export
#'
#' @examples
#' storativity <- 1e-04
#' transmissivity <- 1e-0
#' radius_well <- 0.05
#' frequency <- 10^seq(-5, 2, by = 0.05)
#' tau   <- 1 / frequency
#' hsieh_1987(frequency, storativity, transmissivity, radius_well)
tidal_hsieh_1987 <- function(frequency,
                             storativity,
                             transmissivity,
                             radius_well) {

  omega <- .calc_omega(frequency)
  alpha <- .calc_alpha_w(omega, transmissivity, storativity, radius_well)

  kel   <- kelvin(alpha, nSeq = 2)

  ker_0 <- kel[['ker_0']]
  kei_0 <- kel[['kei_0']]

  ker_1 <- kel[['ker_1']]
  kei_1 <- kel[['kei_1']]

  denom <- (sqrt(2.0) * alpha * (ker_1^2 + kei_1^2))
  phi <- (-ker_1 - kei_1) / denom
  psi <- (-ker_1 + kei_1) / denom

  t1 <- (omega * radius_well^2) / (2.0 * transmissivity)
  e  <- 1 - t1 * (psi * ker_0 + phi * kei_0)
  f  <-     t1 * (phi * ker_0 - psi * kei_0)

  1.0 / (e + f * 1i)  # convert to imaginary

}




## Test the speed of complex generation
# e <- rnorm(10000)
# f <- rnorm(10000)
#
# microbenchmark::microbenchmark(
#   a <- complex(real = e, imaginary = f),
#   b <-(e + f * 1i)
# )



