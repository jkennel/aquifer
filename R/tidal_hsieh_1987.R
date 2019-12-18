#' tidal_hsieh_1987
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
#' data('hsieh_1987_fig_2_3')
#' storativity <- 1e-07
#' transmissivity <- 1e-03
#' radius_well <- 0.05
#' frequency <- 10^seq(-5, 2, by = 0.05)
#' tau   <- 1 / frequency
#' hsieh <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)
#' plot(Mod(response)~dimensionless_frequency, hsieh,
#'  type='l',
#'  log = 'x',
#'  xlim = c(1, 1000))
#' points(response~dimesionless_frequency, hsieh_1987_fig_2_3[variable=='gain' & S == storativity])
#'
#' plot(unwrap(Arg(response)) * 180/pi~dimensionless_frequency, hsieh,
#'  type='l',
#'  log = 'x',
#'  xlim = c(1, 1000),
#'  ylim = c(0, -90))
#' points(response~dimesionless_frequency, hsieh_1987_fig_2_3[variable=='phase' & S == storativity])
#'
tidal_hsieh_1987 <- function(frequency,
                             storativity,
                             transmissivity,
                             radius_well,
                             radius_casing = radius_well) {

  omega <- .calc_omega(frequency)

  # equation 10
  alpha <- .calc_alpha_w(omega, storativity, transmissivity, radius_well)


  kel   <- kelvin(alpha, nSeq = 2)

  ker_0 <- kel[['ker_0']]
  kei_0 <- kel[['kei_0']]

  ker_1 <- kel[['ker_1']]
  kei_1 <- kel[['kei_1']]

  # equations 8 & 9
  denom <- (sqrt(2.0) * alpha * (ker_1^2 + kei_1^2))
  phi <- (-ker_1 - kei_1) / denom
  psi <- (-ker_1 + kei_1) / denom

  # equations 13 & 14
  t1 <- .calc_dimensionless_frequency(omega, radius_casing, transmissivity)
  e  <- 1 - t1 * (psi * ker_0 + phi * kei_0)
  f  <-     t1 * (phi * ker_0 - psi * kei_0)

  # return data.table of frequency and response
  out <- data.table::data.table(frequency = rep(frequency, times = length(transmissivity)), psi, phi, e, f)
  out[, dimensionless_frequency := transmissivity / (frequency * radius_casing^2)]
  #out[, a_prime := (1.0 / storativity) * (e^2 + f^2)^(-0.5)]
  out[, response := 1.0 / (e + f * 1i)]  # convert to imaginary

}



#' fit_tidal_hsieh_1987
#'
#'  This function fits a empirical response function value to the Hsieh model.
#'  For high transmissivity wells (>1e-4 m2/sec) this will likely not give
#'  reasonable storativity values.
#'
#' @param frequency frequency in cycles per second
#' @param gain value to fit in m/ns
#' @param phase phase shift relative to Earth tide
#' @param radius_well radius of the well
#' @param radius_casing radius of the casing
#'
#' @return
#' @export
#'
#' @examples
#' data('hsieh_1987_fig_2_3')
#' storativity <- 1e-07
#' transmissivity <- 1e-05
#' radius_well <- 0.05
#' frequency <- 1.9322736/86400
#' hsieh <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)
#'
#' res <- fit_tidal_hsieh_1987(frequency = 1.9322736/86400,
#'                  Mod(hsieh$response),      # m / strain
#'                  Arg(hsieh$response),      # degrees
#'                  radius_well,
#'                  radius_casing = radius_well)
#' all.equal(res$par, log10(c(storativity, transmissivity)), tolerance = 0.05)
fit_tidal_hsieh_1987 <- function(frequency = 1.9322736/86400,
                                 gain,       # m / nstrain
                                 phase,      # degrees
                                 radius_well,
                                 radius_casing = radius_well) {



  to_minimize <- function(par, gain, phase, frequency, radius_well, radius_casing) {

    amp_phase_cx <- complex(modulus = gain, argument = phase)

    resp <- tidal_hsieh_1987(frequency,
                             10^par[1], # storativity
                             10^par[2], # transmissivity
                             radius_well,
                             radius_casing)$response

    # Calculate the complex difference

    resp <- complex(modulus = Mod(resp) / 10^par[1], argument = Arg(resp))
    # print(amp_phase_cx)
    # print(resp)

    diff <- resp - amp_phase_cx
    # Difference as a real value distance
    Re(diff * Conj(diff))

  }

  par <- c(-5, -5)
  res <- optim(fn = to_minimize,
               par = par,
               lower = c(-8, -8),
               upper = c(-3, 1),
               method = 'L-BFGS-B',
               gain = gain,
               phase = phase,
               frequency = frequency,
               radius_well = radius_well,
               radius_casing = radius_casing)



}



# e <- rnorm(10000)
# f <- rnorm(10000)
#
# microbenchmark::microbenchmark(
#   a <- complex(real = e, imaginary = f),
#   b <-(e + f * 1i)
# )



