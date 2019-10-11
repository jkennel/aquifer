#' tidal_wang_2018
#'
#' @param frequency
#' @param storativity
#' @param transmissivity
#' @param k_vertical
#' @param thickness_confining
#' @param radius_well
#' @param radius_casing
#'
#' @return
#' @export
#'
tidal_wang_2018 <- function(frequency,
                            storativity,
                            transmissivity,
                            k_vertical,
                            thickness_confining,
                            radius_well,
                            radius_casing = radius_well
) {



  omega   <- .calc_omega(frequency)
  t1      <- 1i * omega * storativity
  k_div_b <- k_vertical / thickness_confining
  beta    <- sqrt(k_div_b / (transmissivity) + (t1 / transmissivity))
  beta_rw <- beta * radius_well
  k0      <- Bessel::BesselK(beta_rw, nu = 0, nSeq = 2, expon.scaled = FALSE)

  numer <- radius_casing^2 * 1i  * omega * k0[,1]
  denom <- radius_well   * 2.0 * transmissivity * beta * k0[,2]

  xi    <- 1 + (numer / denom)
  x0    <- t1 / (xi * (t1 + (k_div_b)))

  return(x0)

}



# frequency  <- 1.9322736 / 86400
# transmissivity  <- 1e-6
# storativity  <- 1e-2
# radius_well <- radius_casing <- 0.1
# k_vertical <- 10^(seq(-11, -3, 0.1))
# thickness_confining <- 1
#
# x0 <- wang_2018(frequency,
#                 storativity,
#                 transmissivity,
#                 k_vertical,
#                 thickness_confining,
#                 radius_well,
#                 radius_casing = radius_well)
#
#
# x <- seq(-11, -3, 0.1)
#
#
# plot(y = Mod(x0), x = k_vertical,
#      log = 'xy', col = 'black', type='l',
#      ylim = c(1e-10, 1))
#
# #dat <- fread('/media/kennel/Data/tmp/wang2018_f2_t0_sn2.csv')
# dat <- fread('/media/kennel/Data/tmp/wang2018_f2_tn6_sn2.csv')
#
# x0 <- wang_2018(frequency,
#                 storativity,
#                 1,
#                 k_vertical,
#                 thickness_confining,
#                 radius_well,
#                 radius_casing = radius_well)
# plot(y = (Arg(x0)) * 180/pi,
#      x = log10(k_vertical),
#      col = 'black',
#      type='l',
#      ylim = c(-80, 100))
# points(V2~V1, dat)
#
# x0 <- wang_2018(frequency,
#                 storativity,
#                 1e-6,
#                 k_vertical,
#                 thickness_confining,
#                 radius_well,
#                 radius_casing = radius_well)
# points(y = (Arg(x0)) * 180/pi,
#      x = log10(k_vertical),
#      col = 'black',
#      type='l',
#      ylim = c(-80, 100))
#
#
# x0 <- wang_2018(frequency,
#                 storativity,
#                 1e-8,
#                 k_vertical,
#                 thickness_confining,
#                 radius_well,
#                 radius_casing = radius_well)
# points(y = (Arg(x0)) * 180/pi,
#        x = log10(k_vertical),
#        col = 'black',
#        type='l',
#        ylim = c(-80, 100))
#
#
# x0 <- wang_2018(frequency,
#                 1e-8,
#                 1e-8,
#                 k_vertical,
#                 thickness_confining,
#                 radius_well,
#                 radius_casing = radius_well)
# points(y = (Arg(x0)) * 180/pi,
#        x = log10(k_vertical),
#        col = 'black',
#        type='l',
#        ylim = c(-80, 100))
#
#
# x0 <- wang_2018(frequency,
#                 1e-6,
#                 1e-8,
#                 k_vertical,
#                 thickness_confining,
#                 radius_well,
#                 radius_casing = radius_well)
# points(y = (Arg(x0)) * 180/pi,
#        x = log10(k_vertical),
#        col = 'black',
#        type='l',
#        ylim = c(-80, 100))
#
#
#
# x0 <- wang_2018(frequency,
#                 1e-4,
#                 1,
#                 k_vertical,
#                 thickness_confining,
#                 radius_well,
#                 radius_casing = radius_well)
# points(y = (Arg(x0)) * 180/pi,
#        x = log10(k_vertical),
#        col = 'black',
#        type='l',
#        ylim = c(-80, 100))
