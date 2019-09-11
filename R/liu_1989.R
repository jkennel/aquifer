#' liu_1989
#'
#' @param frequency
#' @param storativity
#' @param transmissivity
#' @param thickness_aquifer
#' @param height_water
#' @param radius_well
#' @param gravity
#'
#' @return
#' @export
#'
#' @examples
tidal_liu_1989 <- function(frequency,
                     storativity,
                     transmissivity,
                     thickness_aquifer,
                     height_water,
                     radius_well,
                     gravity =  9.80665) {

  omega <- .calc_omega(frequency)
  alpha <- .calc_alpha_w(omega, transmissivity, storativity, radius_well)

  kel <- kelvin(alpha, nSeq = 1)

  ker_0 <- kel$ker_0
  kei_0 <- kel$kei_0

  u <- (thickness_aquifer / transmissivity) * (ker_0 + kei_0 * 1i)
  gamma <- sqrt(2.0 * 1i * omega / (radius_well^2 * gravity * u))

  gamma_term <- exp(-gamma * thickness_aquifer)

  e <- -(omega^2 / gravity) * (height_water + (1-gamma_term) / (gamma + gamma*gamma_term))
  f <- -(1i * omega * u * radius_well^2) *
    ((gamma * gamma_term) / (1 - exp(-2.0 * gamma * thickness_aquifer)))

  ef <- 1.0 / (e + f + 1)

  complex(modulus = Mod(ef), argument = -Arg(ef))

}

