
#' cooper_1965
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
tidal_cooper_1965 <- function(frequency,
                        storativity,
                        transmissivity,
                        thickness_aquifer,
                        height_water,
                        radius_well,
                        gravity =  9.80665) {

  height_effective <- .calc_h_w(height_water, thickness_aquifer)
  omega <- .calc_omega(frequency)
  alpha <- .calc_alpha_w(omega, transmissivity, storativity, radius_well)


  t1 <- (radius_well^2 * omega) / (2.0 * transmissivity)
  kel <- kelvin(alpha, nSeq = 1)

  ker_0 <- kel[['ker_0']]
  kei_0 <- kel[['kei_0']]

  e <- 1 - (t1 * kei_0) - ((omega)^2 * height_effective) / gravity
  f <-      t1 * ker_0

  1.0 / (e + f * 1i)  # convert to imaginary
}
