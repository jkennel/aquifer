# Hussein
# Determination of Fluid Flow Properties From the Response of Water Levels in Wells to Atmospheric Loading
#' areal_rojstaczer_semiconfined
#' @inheritParams parameters
#'
#' @return
#' @export
#'
areal_rojstaczer_semiconfined <- function(frequency,
                                          radius_well,
                                          transmissivity,
                                          storage_confining,
                                          storage_aquifer,
                                          diffusivity_confining,
                                          diffusivity_vadose,
                                          thickness_confining,
                                          thickness_vadose,
                                          loading_efficiency,
                                          attenuation) {

  omega  <- .calc_omega(frequency)
  R      <- .calc_dimensionless_frequency(omega, thickness_vadose, diffusivity_vadose)
  Q      <- .calc_dimensionless_frequency(omega, thickness_confining, diffusivity_confining)
  W      <- .calc_W(omega, radius_well, transmissivity)

  sqrt_R <- sqrt(R)

  mn     <- .calc_mn(sqrt_R, attenuation)

  p0     <- ((mn$m - 1i * mn$n) - loading_efficiency) *
    exp(-(1i + 1.0) * sqrt(Q)) + loading_efficiency

  k0     <- Bessel::BesselK(
    ((W^2 * (storage_aquifer^2 + (storage_confining / (2.0 * Q))^2) )^0.25) *
      exp(0.5 * 1i * atan(2.0 * Q)), 0)  # well/aquifer

  #W <- 0#ifelse(Mod(W * k0) < 1e-10, 0.0, W)
  x0 <- (-1 + p0) / (1 + (1i * 0.5 * W * k0))

  return(data.table(frequency, R, Q, W, Q_div_W = Q/W, R_div_Q = R/Q, response = x0))
}

# x <- seq(0, 2*pi * 12, pi/90)
# baro <- sin(x)
# p <- 0.6 * sin(x)
# wl <- p-baro
# plot(baro, type='l', col = 'red')
# points(p, type='l', col = 'black')
# points(wl, type = 'l', col = 'green')
#
# lm_baro <- lm(baro~sin(x) + cos(x))
# lm_p <- lm(p~sin(x) + cos(x))
# lm_wl <- lm(wl~sin(x) + cos(x))
#
# sqrt(sum(coef(lm_p)[2:3]^2))
# sqrt(sum(coef(lm_wl)[2:3]^2))
#
# atan2(coef(lm_p)[3], coef(lm_p)[2]) * 180/pi
# atan2(coef(lm_wl)[3], coef(lm_wl)[2]) * 180/pi

# x <- complex(real = -0.5, imaginary = -0.5)
# sqrt(Re(x)^2+Im(x)^2)
#
#
# microbenchmark::microbenchmark(
#   atan2(Im(x), Re(x)),
#   Arg(x)
# )
# # This has been tested for a single value using octave
#
# #library(aquifer)
# library(Bessel)
# # rw <- 0.1
# # transmissivity <- 0.01
# frequency <- 10^seq(-0, 6, length.out = 100000)
# # bcon <- 1
# # dcon = 0.05
# # lunsat <- 1
# # dunsat <- 10
# # Tc <- 1
# # be <- 0.5
# # Scon <- 1e-8
# # Saqu <- 1e-8
#
# rw  <- radius_well   <- 0.1
# transmissivity <- 20
#
# thickness_confining <- bcon   <- 10
# diffusivity_confining <- diffusivity_vadose <- dcon   <- dunsat <- 100
# thickness_vadose <- lunsat <- 0.1
# attenuation <- Tc     <- 1
# be     <- 0.5
# loading_efficiency <- 1 - be
# storage_confining <- Scon   <- 1e-3
# storage_aquifer <- Saqu   <- 1e-4
#
#
#
#
#
# #
# # calc_qw <- function(transmissivity, bcon, rw, dcon) {
# #   ((transmissivity)*(bcon^2)) / (2 * (dcon)*(rw^2));
# # }
# #
# # calc_rq <- function(lunsat, dunsat, bcon, dcon) {
# #   ((lunsat^2)*dcon)/(dunsat*(bcon^2));
# # }
# #
# # calc_Q <- function(qw, w) {
# #   qw * w
# # }
# #
# # calc_q <- function(Q, Scon) {
# #   (2 * Q / Scon)
# # }
# #
# # calc_r <- function(rq, q) {
# #   rq*q
# # }
#
#
#
# calc_omega <- function(frequency) {
#   (2 * pi * frequency)
# }
#
# calc_w <- function(omega, rw, transmissivity) {
#   (rw^2 * (omega)) / (transmissivity)
# }
#
# calc_r <- function(omega, lunsat, dunsat) {
#   (lunsat^2 * omega) / (2.0 * dunsat)
# }
#
# calc_q <- function(omega, bcon, dcon) {
#   (bcon^2 * omega) / (2.0 * dcon)
# }
#
# calc_m <- function(Tc, sqrtR) {
#   Tc*(2*cosh(sqrtR)*cos(sqrtR))/(cosh(2*(sqrtR))+cos(2*(sqrtR)))
# }
#
# calc_n <- function(Tc, sqrtR) {
#   Tc*(2*sinh(sqrtR)*sin(sqrtR))/(cosh(2*(sqrtR))+cos(2*(sqrtR)))
# }
#
# calc_p0 <- function(m, n, be, Q) {
#   ((m + 1i * n)-(1 - be)) * exp(-(1i + 1) * sqrt(Q)) + (1 - be);
# }
#
# calc_k0 <- function(W, Saqu, Scon, Q) {
#   BesselK(((W^2 * (Saqu^2 + (Scon / (2 * Q))^2) )^0.25) *
#                     exp(0.5 * 1i * atan(2 * Q)), 0)
# }
#
# calc_x1 <- function(p0, W, k0) {
#   (-1+p0) / (1+(1i * 0.5 * W * k0))
# }
#
# rojstaczer <- function(frequency,
#                        radius_well,
#                        transmissivity,
#                        storage_confining,
#                        storage_aquifer,
#                        diffusivity_confining,
#                        diffusivity_vadose,
#                        thickness_confining,
#                        thickness_vadose,
#                        loading_efficiency,
#                        attenuation) {
#
#   omega <- (2 * pi * frequency)
#
#   sqrt_r <- sqrt((thickness_vadose^2 * omega) / (2.0 * diffusivity_vadose))
#   q      <- (thickness_confining^2 * omega) / (2.0 * diffusivity_confining)
#   w      <- (radius_well^2 * (omega)) / (transmissivity)
#
#   denom  <- (cosh(2.0 * (sqrt_r)) + cos(2.0 * (sqrt_r))) / attenuation
#   m      <- (2.0 * cosh(sqrt_r) * cos(sqrt_r)) / denom
#   n      <- (2.0 * sinh(sqrt_r) * sin(sqrt_r)) / denom
#
#   p0     <- ((m + 1i * n) - loading_efficiency) * exp(-(1i + 1.0) * sqrt(q)) + loading_efficiency;
#   k0     <- BesselK(((w^2 * (storage_aquifer^2 + (storage_confining / (2.0 * q))^2) )^0.25) *
#                                exp(0.5 * 1i * atan(2 * q)), 0)  # well/aquifer
#
#   (-1 + p0) / (1 + (1i * 0.5 * w * k0))
#
# }
#
# microbenchmark::microbenchmark(
#   a <- rojstaczer(frequency,
#              radius_well,
#              transmissivity,
#              storage_confining,
#              storage_aquifer,
#              diffusivity_confining,
#              diffusivity_vadose,
#              thickness_confining,
#              thickness_vadose,
#              loading_efficiency,
#              attenuation),
#   times = 2
# )
#
# hussein_gain <- fread('/media/kennel/Data/tmp/gain.csv')
# setnames(hussein_gain, c('frequency', 'gain'))
#
# frequency <- hussein_gain$frequency
# use_data(hussein_gain)
#
# microbenchmark::microbenchmark({
#
# omega <- calc_omega(frequency)
# Q  <- calc_q(omega, bcon, dcon)
# sqrtR  <- sqrt(calc_r(omega, lunsat, dunsat))
# W  <- calc_w(omega, rw, transmissivity)
# M  <- calc_m(Tc, sqrtR)
# N  <- calc_n(Tc, sqrtR)
# p0 <- calc_p0(M, N, be, Q)
# k0 <- as.vector(calc_k0(W, Saqu, Scon, Q))
# x1 <- calc_x1(p0, W, k0)
# },
# times = 2)
#
# plot(gain~frequency, hussein_gain, log = 'x')
# points(Mod(x1)~(frequency), type='l', col = 'red', log = 'x', xlim = c(1e-4, 10))
# abline(v = 1)
# abline(v = 10)
# abline(h = 0.1)
# (Q/W)[1]
# (sqrtR^2/Q)[1]
#
# phase <- (atan2(Im(x1), Re(x1)) * 180/pi)
# phase <- ifelse(phase < 0, phase, phase - 360)
# plot(phase~W, type='l', col = 'red', log = 'x', xlim = c(1e-4, 10), ylim = c(-260, -120))
#
#
# mean(abs(phase - hussein_phase$phase))
#
#
# mean(abs(Mod(x1) - hussein_gain$gain))
#
#
#
# #
# # W <- calc_w(rw, frequency, transmissivity)
# #
# # omega <- calc_omega(frequency)
# # QW <- calc_qw(transmissivity, bcon, rw, dcon)
# # RQ <- calc_rq(lunsat, dunsat, bcon, dcon)
# #
# # Q <- calc_Q(QW, W)
# # R <- calc_r(RQ, Q)
# # m <- calc_m(Tc, R)
# # n <- calc_n(Tc, R)
# #
# # p0 <- calc_p0(m, n, be, Q)
# # q <- calc_q(Q, Scon)
# # k0 <- calc_k0(W, Saqu, Q)
# # x1 <- calc_x1(p0, W, k0)
#
# # library(data.table)
#
#
# hussein_phase <- fread('/media/kennel/Data/tmp/phase.csv')
# setnames(hussein_phase, c('frequency', 'phase'))
# use_data(hussein_phase)
#
# frequency <- hussein_phase$frequency
#
# omega <- calc_omega(frequency)
# Q  <- calc_q(omega, bcon, dcon)
# sqrtR  <- sqrt(calc_r(omega, lunsat, dunsat))
# W  <- calc_w(omega, rw, transmissivity)
# M  <- calc_m(Tc, sqrtR)
# N  <- calc_n(Tc, sqrtR)
# p0 <- calc_p0(M, N, be, Q)
# k0 <- calc_k0(W, Saqu, Scon, Q)
# x1 <- calc_x1(p0, W, k0)
#
# plot(phase~frequency, hussein_phase, log = 'x')
# phase <- (atan2(Im(x1), Re(x1)) * 180/pi)
# phase <- ifelse(phase < 0, phase, phase - 360)
# points(phase~frequency, type='l', col = 'red')
# mean(abs(phase - hussein_phase$phase))
#
#
#
# layout(matrix(1:2, ncol= 1))
# par(mar = c(5,5,0.1,0.1))
# plot(Mod(x1)~frequency, type='l', col = 'red', log = 'x')
#
# # print((R/Q)[1])
# # print((Q/W)[1])
#
#
# abline(v = 100)
# abline(v = 1e-4)
# abline(h = be, lty = 2)
# phase <- (atan2(Im(x1), Re(x1)) * 180/pi)
# phase <- ifelse(phase < 0, phase, phase - 360)
# points(phase~frequency, type='l')
# abline(v = 100)
# abline(h = -250, lty = 2)
# abline(h = -200, lty = 2)
