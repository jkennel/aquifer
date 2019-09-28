#' liu_1989
#'
#' @param frequency
#' @param storativity
#' @param transmissivity
#' @param thickness_aquifer
#' @param height_water
#' @param radius_well
#' @param radius_casing
#' @param gravity
#'
#' @return
#' @export
#'
#' @examples
#' data('liu_1989_fig_8')
#' storativity <- 5e-4
#' transmissivity <- 0.5
#' radius_well <- 0.117
#' tau   <- seq(10, 50, 0.001)
#' frequency <- 1/tau
#' liu    <- tidal_liu_1989(frequency, storativity, transmissivity, thickness_aquifer = 565, height_water = 92, radius_well, radius_casing = radius_well/2)
#' cooper <- tidal_cooper_1965(frequency, storativity, transmissivity, thickness_aquifer = 565, height_water = 92, radius_well, radius_casing = radius_well/2)
#' plot(response~period, liu_1989_fig_8, type='p')
#' points(Mod(response)~(tau), liu,
#'  type='l',
#'  ylim = c(0.0, 8), col = 'red')
#' points(Mod(response)~tau, cooper,
#'  type='l',
#'  col = 'blue')
#'
#' plot(unwrap(Arg(response)) * 180/pi~tau, liu,
#'  type='l',
#'  log = 'x')
#' points(unwrap(Arg(response)) * 180/pi~tau, cooper,
#'  type='l',
#'  col = 'blue')
tidal_liu_1989 <- function(frequency,
                           storativity,
                           transmissivity,
                           thickness_aquifer,
                           height_water,
                           radius_well,
                           radius_casing = radius_well,
                           gravity =  9.80665) {

  omega <- .calc_omega(frequency)
  alpha <- .calc_alpha_w(omega, storativity, transmissivity, radius_well)


  kel   <- kelvin(alpha, nSeq = 1)

  k_0   <- kel[['k_0']]
  # ker_0 <- kel[['ker_0']]
  # kei_0 <- kel[['kei_0']]

  # Liu equation A13
  u <- (thickness_aquifer / transmissivity) * (k_0)

  # Liu equation A16
  beta <- sqrt((2.0 * 1i * omega) / (radius_well^2 * gravity * u))

  # Liu equation A20
  beta_term   <- exp(      -beta * thickness_aquifer)
  beta_term_2 <- exp(2.0 * -beta * thickness_aquifer)

  e <- -(omega^2 / gravity) *
    (height_water + (1.0 - beta_term) / (beta * (1.0 + beta_term))) + 1.0

  f <- -(1i * omega * u * radius_well^2) *
    ((beta * beta_term) / (1 - beta_term_2))


  out <- data.table::data.table(frequency)
  out[, dimensionless_frequency := transmissivity / (frequency * radius_casing^2)]
  out[, Q := .calc_dimensionless_frequency(omega, height_water, transmissivity/storativity)]


  # Here we make a modification so that the modulus and phase are correct.
  #complex(modulus = Mod(1/(e+f)), argument = Arg(e+f))
  out[, response := complex(modulus = Mod(1 / (e + f)),
                            argument = -Arg(e - f))]  # convert to imaginary

}


# tidal_liu_1989 <- function(frequency,
#                            storativity,
#                            transmissivity,
#                            thickness_aquifer,
#                            height_water,
#                            radius_well,
#                            radius_casing = radius_well,
#                            gravity =  9.80665) {
#
#   omega <- .calc_omega(frequency)
#   alpha <- .calc_alpha_w(omega, storativity, transmissivity, radius_well)
#
#
#   kel   <- kelvin(alpha, nSeq = 1)
#
#   k_0   <- kel[['k_0']]
#   # ker_0 <- kel[['ker_0']]
#   # kei_0 <- kel[['kei_0']]
#
#   # Liu equation A13
#   u <- (thickness_aquifer / transmissivity) * (k_0)
#
#   # Liu equation A16
#   beta <- sqrt((2.0 * 1i * omega) / (radius_well^2 * gravity * u))
#
#   # Liu equation A20
#   beta_term   <- exp(      -beta * thickness_aquifer)
#   beta_term_2 <- exp(2.0 * -beta * thickness_aquifer)
#
#   e <- -(omega^2 / gravity) *
#     (height_water + (1.0 - beta_term) / (beta * (1.0 + beta_term))) + 1.0
#
#   f <- -(1i * omega * u * radius_well^2) *
#     ((beta * beta_term) / (1 - beta_term_2))
#
#
#   out <- data.table::data.table(frequency)
#   out[, dimensionless_frequency := transmissivity / (frequency * radius_casing^2)]
#   out[, Q := .calc_dimensionless_frequency(omega, height_water, transmissivity/storativity)]
#   out[, response := 1.0 / (e + f)]  # convert to imaginary
#
# }


