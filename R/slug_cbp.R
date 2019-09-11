#' slug_cbp
#'
#' @description
#' This function calculates H/H0 for a well of finite diameter using equation 7
#' from Cooper, Bredehoeft and Papdopulos, 1967
#' Response of a Finite-Diameter Well to an Instantaneous Charge Water.
#'
#' This function uses Stehfest method for the Inverse Laplace Transform.
#'
#' @param time elapsed time since slug introduction
#' @param radius_casing radius of the casing where the water level is
#' @param radius_screen radius of the screened interval
#' @param radius radius from borehole center to calculate the response
#' @param storativity aquifer storativity
#' @param transmissivity aquifer transmissivity
#' @param head_0 height of introduced head change
#' @param n coefficient for Stehfest algorithm (should be < 20)
#'
#' @return vector of H/H0
#' @export
#'
#' @examples
#' t <- 5.776 * 10^(seq(-4, 3, 0.01))
#' s <- slug_cbp(t, radius_screen = 7.6)
#' plot(s~t, log = 'x', type = 'l')
slug_cbp <- function(time,
                     radius_casing = NULL,
                     radius_screen = NULL,
                     radius = NULL,
                     storativity = 1e-5,
                     transmissivity = 10,
                     head_0 = 1,
                     n = 12L) {

  if (is.null(radius_casing) & !is.null(radius_screen)) {
    radius_casing <- radius_screen
  }

  if (!is.null(radius_casing) & is.null(radius_screen)) {
    radius_screen <- radius_casing
  }

  if (is.null(radius)) {
    radius <- radius_screen
  }

  stehfest(time = time,
           n = n,
           impulse = slug_cbp_parallel,
           radius_casing = radius_casing,
           radius_screen = radius_screen,
           radius = radius,
           storativity = storativity,
           transmissivity = transmissivity,
           head_0 = head_0)



}

.slug_cbp_laplace <- function(p,
                              radius_casing = NULL,
                              radius_screen = NULL,
                              radius = NULL,
                              storativity = 1e-5,
                              transmissivity = 1e-2,
                              head_0 = 1) {



  alpha <- (radius_screen^2 * storativity) / (radius_casing^2)

  q <- sqrt((p * storativity) / transmissivity)
  radius_screen_q <- radius_screen * q

  if (radius == radius_screen) {
    bess_rs <- bess_r <- bessel_k_parallel(radius_screen_q, 0L)
  } else {
    bess_rs <- bessel_k_parallel(radius_screen_q, 0L)
    bess_r  <- bessel_k_parallel(radius * q, 0L)
  }

  # Cooper, Bredehoeft and Papdopulos, 1967
  # Response of a Finite-Diameter Well to an Instantaneous Charge Water
  # laplace space solution eq 7

  # bessel function from boost
  (radius_screen * storativity * head_0 * bess_r) /
    ((transmissivity * q) *
       ((radius_screen_q * bess_rs) +
          (2 * alpha * bessel_k_parallel(radius_screen_q, 1L))))

}



# The complex implementation is slow.
.slug_cbp_laplace_complex <- function(p,
                              radius_casing = NULL,
                              radius_screen = NULL,
                              radius = NULL,
                              storativity = 1e-5,
                              transmissivity = 1e-2,
                              head_0 = 1) {


  require(Bessel)

  alpha <- (radius_screen^2 * storativity) / (radius_casing^2)

  q <- sqrt((p * storativity) / transmissivity)
  radius_screen_q <- radius_screen * q

  if (radius == radius_screen) {
    bess_rs <- bess_r <- BesselK(radius_screen_q, 0L)
  } else {
    bess_rs <- BesselK(radius_screen_q, 0L)
    bess_r  <- BesselK(radius * q, 0L)
  }

  # Cooper, Bredehoeft and Papdopulos, 1967
  # Response of a Finite-Diameter Well to an Instantaneous Charge Water
  # laplace space solution eq 7

  # bessel function from boost
  (radius_screen * storativity * head_0 * bess_r) /
    ((transmissivity * q) *
       ((radius_screen_q * bess_rs) +
          (2 * alpha * BesselK(radius_screen_q, 1L))))

}



