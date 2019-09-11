# frequency                <- 10^seq(-12, 3, length.out = 100)
# thick <- 10
# d <- thickness_aquifer <- thick
# thickness_saturated_well <- d
# thickness_vadose <- thick
# diffusivity_vertical  <- 0.01
# diffusivity_vadose <- d_prime <- 0.0001
#
# z <- thick
# loading_efficiency <- 0.5
# attenuation <- 1
# sy <- specific_yield <- 0.2
# ss <- storage_aquifer <- 0.001
# kz <- k_vertical <- 0.0001
# height_water <- thickness_saturated_well
#
#
#
#
#
#
# omega <- .calc_omega(frequency)
# R     <- .calc_R(omega, thickness_vadose, diffusivity_vadose)
# Q     <- .calc_Q(omega, height_water, diffusivity_vertical)
# Qu    <- .calc_Q(omega, thickness_saturated_well, diffusivity_vertical)
# # Q  <- (omega * height_water^2/(2*diffusivity_vertical))
# # Qu <- (omega * thickness_saturated_well^2/(2*diffusivity_vertical))
# sqrt_R  <- sqrt(R)
# sqrt_Qu <- sqrt(Qu)
# sqrt_Q  <- sqrt(Q)
#
# mn      <- .calc_mn(sqrt_R, attenuation)
# ohm     <- .calc_ohm(omega, storage_aquifer, k_vertical, specific_yield)
#
#
# # Figure 2 WORKS
#
# # library(data.table)
# # dat <- fread('/media/kennel/Data/tmp/roj_1990_fig2.csv')
# # setnames(dat, c('x', 'y'))
# # As <- 0.05
# # p0 <- -(exp(-(1 + 1i) * sqrt(Qu)) - 1)
# # p02 <- 1/(1-ohm)-1
# # plot(y~x, dat, log='x')
# # points(Re(ohm / (1-1i)), Mod(p02), type='l')
#
#
#
# # Figure 3
# ohm <- 0
# h1_h2   <- .calc_H1_H2(omega, ohm, thickness_aquifer, diffusivity_vertical)
#
# # p0 <- exp(-(1i + 1.0) * sqrt_Qu) / h1_h2$h1 +
# #       exp( (1i + 1.0) * sqrt_Qu) / h1_h2$h2 - 1
#
# t1 <- (1i + 1.0) * sqrt(Q)
# p0 <- ((mn$m - 1i * mn$n) - loading_efficiency) *
#        (exp(-t1) / h1_h2$h1 +
#         exp( t1) / h1_h2$h2) + loading_efficiency
#
#
# plot(Qu, Mod(-1+p0), type='l', log = 'x', xlim = c(1e-3, 100), ylim = c(0, 1.5))
# R/Qu
#
# library(data.table)
# dat <- fread('/media/kennel/Data/tmp/roj_1990_fig3p.csv')
# setnames(dat, c('x', 'y'))
# plot(Qu, unwrap(Arg(p0)) * 180/pi, type='l', log = 'x', xlim = c(1e-3, 100))
#
# points(y~x, dat, log='x')
# thickness_saturated_well/thickness_aquifer
#
# plot(Qu, unwrap(Arg((p0))) * 180/pi, type='l', log = 'x', xlim = c(1e-3, 100))
#
# # u_v     <- .calc_u_v(sqrt_Qu, h1_h2)
# # p0 <- (u_v$u + 1i * u_v$v)
#
#
#
# #Figure 4
# library(data.table)
# dat <- fread('/media/kennel/Data/tmp/roj_1990_fig4g.csv')
# setnames(dat, c('x', 'y'))
#
#
# t1 <- (1i + 1.0) * sqrt(Q)
# p0 <- ((mn$m - 1i * mn$n) - loading_efficiency) *
#   (exp(-t1) / (h1_h2$h1) +
#    exp( t1) / (h1_h2$h2)) + loading_efficiency
#
#
# plot(Qu, Mod(p0-1), type='l', log = 'x', xlim = c(1e-3, 100), ylim = c(0, 1.5))
# points(y~x, dat, log='x')
#
#
# #plot(Qu, unwrap(Arg(p0)) * 180/pi, type='l', log = 'x', xlim = c(1e-3, 100))
#
#
# R/Qu
#
#
#
# #
# # p0 <- ((mn$m + 1i * mn$n) - loading_efficiency) *
# #   (exp(-(1i + 1.0) * sqrt(Q)) / h1_h2$h1 +
# #    exp( (1i + 1.0) * sqrt(Q)) / h1_h2$h2) + loading_efficiency
# #
# # plot(Qu, Mod(p0), log = 'x', ylim = c(0, 1.5), type='l')
# # R/Qu
# #
# # plot(Qu, unwrap(Arg(p0)) * 180/pi, log = 'x')
# #
#
# #
# #
# #
# #
# #
# # ohm_prime <- (1.0 - 1i) * ((storage_aquifer * k_vertical) /
# #                            (2.0 * specific_yield^2 * omega))^0.5
# #
# #
# #
# # h2 <- 1.0 + exp(2.0 * (1i + 1) * (omega * d^2 / (2.0 * d_prime))^0.5) +
# #   ohm_prime * (1 -exp(2.0 * (1i + 1) * (omega * d^2 / (2.0 * d_prime))^0.5))
# #
# #
# # u_bar <- exp(-sqrt_Qu * (-cos(sqrt_Qu) + sin(sqrt_Qu))/ (2.0 * sqrt_Qu * h1)) +
# #          exp( sqrt_Qu * ( cos(sqrt_Qu) + sin(sqrt_Qu))/ (2.0 * sqrt_Qu * h1)) +
# #   ((1/h1) - (1/h2)) / (2.0 * sqrt_Qu)
# #
# # v_bar <- exp(-sqrt_Qu * (cos(sqrt_Qu) + sin(sqrt_Qu))/ (2.0 * sqrt_Qu * h1)) +
# #          exp( sqrt_Qu * (-cos(sqrt_Qu) + sin(sqrt_Qu))/ (2.0 * sqrt_Qu * h1)) +
# #   ((1/h2) - (1/h1)) / (2.0 * sqrt_Qu)
# #
# # # p0 <- ((m + 1i * n) - loading_efficiency) *
# # #   (exp(-(1i + 1.0) * sqrt(q)) / h1 +
# # #    exp( (1i + 1.0) * sqrt(q)) / h2) + loading_efficiency
# #
# #
# # p0 <- ((m + 1i * n) - loading_efficiency) *
# #   (u_bar + 1i * v_bar) + loading_efficiency
# #
# #
# #
# # x1 <- p0-1
# #
# # x <- r/qu
# # phase <- Arg(x1)
# # phase <- ifelse(phase > 1, phase - 2*pi, phase) * 180/pi
# # plot(Mod(x1)~qu, type = 'l', log = 'x')
# # #plot(phase~qu, type = 'l', log = 'x')
# #
# # r/qu
