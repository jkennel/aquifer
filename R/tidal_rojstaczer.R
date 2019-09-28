# Intermediate Period Response of Water Wells to Crustal Strain
# Rojstaczer, 1988
# A3a, A3b, A4a, A4b

#' @inheritParams parameters
#' @export
tidal_rojstaczer_1988 <- function(frequency,
                                  storativity,
                                  transmissivity,
                                  height_water) {

  omega <- .calc_omega(frequency)

  # Equation 11
  Q <- .calc_dimensionless_frequency(omega,
                                     height_water,
                                     transmissivity/storativity)

  sqrt_Q <- sqrt(Q)

  # simplified version in complex notation from Rojstaczer & Riley 1990 Equation 7
  data.table(frequency,
             Q,
             response = exp(-(1i + 1.0) * sqrt_Q) - 1.0)

}


#' @inheritParams parameters
#' @export
tidal_rojstaczer_unconfined <- function(frequency,
                                        radius_well,
                                        k_vertical,
                                        storage_aquifer,
                                        specific_yield,
                                        diffusivity_vertical,
                                        diffusivity_vadose,
                                        thickness_aquifer,
                                        thickness_vadose,
                                        thickness_saturated_well
                                        ) {

  omega <- .calc_omega(frequency)
  R     <- .calc_dimensionless_frequency(omega, thickness_vadose, diffusivity_vadose)
  Qu    <- .calc_dimensionless_frequency(omega, thickness_saturated_well, diffusivity_vertical)

  sqrt_R  <- sqrt(R)
  sqrt_Qu <- sqrt(Qu)

  ohm     <- .calc_ohm(omega, storage_aquifer, k_vertical, specific_yield)
  h1_h2   <- .calc_H1_H2(omega, ohm, thickness_aquifer, diffusivity_vertical)
  uv      <- .calc_u_v(sqrt_Qu, h1_h2[['h1']], h1_h2[['h2']])


  # Rojstaczer & Riley 1990 Equation 8
  data.table(frequency,
             R,
             Qu,
             response = -(uv$u + 1i * uv$v) + 1)

}



#
#
# # Does not work -------------------------------------------------------------------
# frequency <- 10^seq(-8, 6, 0.1)
# radius_well <- 0.05
# transmissivity <- 1e-3
# storage_confining <- 1e-5
# storage_aquifer <- 1e-5
# specific_yield <- 0.0001
# diffusivity_confining <- 1
# k_vertical <- 1e-4
# diffusivity_vadose <- 100
# thickness_saturated_well <- 1
# thickness_confining <- 1
# thickness_vadose <- 10
# loading_efficiency <- 0.5
# attenuation <- 1
#
# #Figure 3
# tmp2 <- tidal_rojstaczer_1988(frequency,
#                         storativity,
#                         transmissivity,
#                         height_water)
# tmp <- tidal_riley_1990(frequency,
#                         radius_well,
#                         transmissivity,
#                         storage_confining,
#                         storage_aquifer,
#                         specific_yield,
#                         k_vertical,
#                         diffusivity_confining,
#                         diffusivity_vadose,
#                         thickness_saturated_well,
#                         thickness_confining,
#                         thickness_vadose,
#                         loading_efficiency,
#                         attenuation,
#                         inverse = TRUE)
#
#
# dat <- fread('/media/kennel/Data/tmp/roj_1990_fig3p.csv')
# setnames(dat, c('x', 'y'))
# #plot(y~x, dat, log='x')
# plot(Mod(response)~Q, tmp, type='l', log = 'x', ylim = c(0, 1.5), xlim = c(1e-3, 100))
# (tmp$R/tmp$Q)[1]
#
# plot(unwrap(Arg(response))*180/pi~(Q), tmp, type='l', log = 'x')
# points(180+unwrap(Arg(response))*180/pi~(Q_prime), tmp2, type='l', log = 'x')

# points(180+unwrap(Arg(t1))*180/pi~Q_prime, tmp, type='l', log = 'x', col = 'red')
# points(180+unwrap(Arg(t2))*180/pi~Q_prime, tmp, type='l', log = 'x', col = 'blue')

#plot(0.5 * Mod(response)~Q_prime, tmp, type='l', log = 'x', xlim = c(0.001, 100))

# tmp[!is.nan(response)]
# plot(unwrap(Arg(response))*180/pi~Q_prime, tmp, type='l', log = 'x', xlim = c(0.001, 100))
# Works -------------------------------------------------------------------
# Rojstaczer Figure 3
# Intermediate period response of water levels in wells to crustal strain: sensitivity and noise level
# frequency <- 10^seq(-8, 6, 0.1)
# storativity <- 1e-5
# transmissivity <- 1e-4
# height_water <- 50
# tmp <- tidal_rojstaczer_1988(frequency,
#                  storativity,
#                  transmissivity,
#                  height_water)
# library(data.table)
# gain <- fread('/media/kennel/Data/tmp/rojstaczer_tidal_gain.csv')
# setnames(gain, c('Q_prime', 'gain'))
# plot(gain~Q_prime, gain, log = 'x')
# points(Mod(response)*0.05~Q_prime, tmp, type='l')
#
#
#
#
# phase <- fread('/media/kennel/Data/tmp/rojstaczer_tidal_phase.csv')
# setnames(phase, c('Q_prime', 'phase'))
# plot(phase~Q_prime, phase, log = 'x')
# points(unwrap(Arg(response))*180/pi~Q_prime, tmp, type='l', log = 'x')
#
