test_that("liu amplitude works", {
  library(data.table)
  data('liu_1989_fig_8')
  thickness_aquifer <- 10
  amp_dat           <- liu_1989_fig_8[aquifer_thickness == thickness_aquifer & method == 'liu']
  tau               <- amp_dat$period
  frequency         <- 1.0 / tau
  transmissivity    <- 2.19024
  storativity       <- 1e-4
  height_water      <- 100
  radius_well       <- 0.117
  liu    <- tidal_liu_1989(frequency,
                           storativity,
                           transmissivity,
                           thickness_aquifer,
                           height_water,
                           radius_well,
                           radius_well)

  # plot(Mod(response)~tau, liu, type = 'l')
  # points(10^response~period,
  #        amp_dat,
  #        type='p', pch = 20, cex = 0.4)

  d <- mean(abs((Mod(liu$response) - 10^amp_dat$response)))

  expect_lt(d, 0.25)


  thickness_aquifer <- 50
  amp_dat           <- liu_1989_fig_8[aquifer_thickness == thickness_aquifer & method == 'liu']
  tau               <- amp_dat$period
  frequency         <- 1.0 / tau
  liu    <- tidal_liu_1989(frequency,
                           storativity,
                           transmissivity,
                           thickness_aquifer,
                           height_water,
                           radius_well,
                           radius_well)
  d <- mean(abs((Mod(liu$response) - 10^amp_dat$response)))

  expect_lt(d, 0.3)


  thickness_aquifer <- 100
  amp_dat           <- liu_1989_fig_8[aquifer_thickness == thickness_aquifer & method == 'liu']
  tau               <- amp_dat$period
  frequency         <- 1.0 / tau
  liu    <- tidal_liu_1989(frequency,
                           storativity,
                           transmissivity,
                           thickness_aquifer,
                           height_water,
                           radius_well,
                           radius_well)
  d <- mean(abs((Mod(liu$response) - 10^amp_dat$response)))

  expect_lt(d, 0.3)


  thickness_aquifer <- 500
  amp_dat           <- liu_1989_fig_8[aquifer_thickness == thickness_aquifer & method == 'liu']
  tau               <- amp_dat$period
  frequency         <- 1.0 / tau
  liu    <- tidal_liu_1989(frequency,
                           storativity,
                           transmissivity,
                           thickness_aquifer,
                           height_water,
                           radius_well,
                           radius_well)
  d <- mean(abs((Mod(liu$response) - 10^amp_dat$response)))

  expect_lt(d, 0.3)

  })





#
# library(kitagawa)
#
# S. <- 1e-5    # Storativity [nondimensional]
# T. <- 1e-4    # Transmissivity [m**2 / s]
# D. <- T./S.   # Diffusivity [m**2 / s]
# Ta <- 50      # Aquifer thickness [m] #100
# Hw <- z <- 50 # Depth to water table [m] #10
#
# # Using ANO1 stats from Kit Tbl 1
# Rc. <- 0.075		# Radius of cased portion of well [m]
# Lc. <- 570		# Length of cased portion of well [m]
# Rs. <- 0.135		# Radius of screened portion of well [m]
# Ls. <- 15		# Length of screened portion of well [m]
# Vw. <- sensing_volume(Rc., Lc., Rs., Ls.) 	# volume of fluid [m**3]
# #
# # parameters assumed by well_response:
# #	rho=1000		# density of rock [kg/m**3]
# #	Kf=2.2e9		# Bulk modulus of fluid [Pascals]
# #	grav=9.81	# gravitational acceleration [m/s**2]
# rhog <- 9.81*1000
# # Kitagawa Fig 7: Ku B / Kw Aw = 3 => Aw==4.8 at 40GPa
# Ku. <- 40e9		# Bulk modulus [Pascals]
# B. <- 0.5		# Skemptons ratio [nondimensional]
#
# Q <- 10**seq(-5,3,by=0.05)					# [nondimensional]
# lQ <- log10(Q)
# omega <- omega_norm(Q, z, D., invert=TRUE)		# [Hz]
#
# Phase <- function(Z){
#   Phs. <- Arg(Z) # will wrap to -pi/pi
#   uPhs. <- signal::unwrap(Phs., tol=pi/30)
#   return(data.frame(Phs=Phs., uPhs=uPhs.))
# }
#
# # Responses converted to pressure if TRUE
# asP <- FALSE
# ZasP <- FALSE
#
# wrsp <- open_well_response(omega, T.=T., S.=S., Ta=Ta, Hw=Hw,
#                            model = "cooper", as.pressure=ZasP)
# plot(wrsp)
# crsp <- wrsp[["Response"]][,2]
# cGain <- Mod(crsp)
# cP <- Phase(crsp)
#
#
# wrsp <- open_well_response(omega, T.=T., S.=S., Ta=Ta, Hw=Hw, model = "liu", as.pressure=ZasP)
# plot(wrsp)
# lrsp <- wrsp[["Response"]][,2]
# lGain <- Mod(lrsp)
# lP <- Phase(lrsp)
#
#
# library(aquifer)
# library(data.table)
# thickness_aquifer <- 50
# frequency         <- omega
# transmissivity    <- 1e-4
# storativity       <- 1e-5
# height_water      <- 50
# radius_well       <- (8/12) * (1200/3937)
#
# liu    <- tidal_liu_1989(omega/(2*pi),
#                          storativity,
#                          transmissivity,
#                          thickness_aquifer,
#                          height_water,
#                          radius_well,
#                          radius_well)
# #plot(Mod(response)~frequency, liu, type = 'l', log = 'xy')
#
# liu_2    <- tidal_liu_1989_2(omega/(2*pi),
#                          storativity,
#                          transmissivity,
#                          thickness_aquifer,
#                          height_water,
#                          radius_well,
#                          radius_well)
#
#
#
# unwrap(Arg(lrsp)) * 180/pi
#
# unwrap(Arg(liu$response)) * 180/pi
#
# head(lrsp)
# head(liu$response)
# head(liu_2$response)
#
# plot(Mod(lrsp), log = 'y', type='l')
# points(Mod(liu_2$response), col = 'blue', log = 'y', type='l')
# points(Mod(cooper$response), col = 'red', type = 'l')
#
# all.equal(Mod(lrsp), Mod(liu$response))
# all.equal(Mod(lrsp), Mod(liu_2$response))
#
# tail(lrsp)
# tail(liu$response)
# tail(liu_2$response)
#
# plot(unwrap(Arg(liu_2$response)) * 180/pi, type='l')
# points(unwrap(Arg(lrsp)) * 180/pi, type='l')
# points(unwrap(Arg(cooper$response)) * 180/pi, type='l', col = 'red')
#
#
#
#
# cooper    <- tidal_cooper_1965(omega / (2 * pi),
#                                storativity,
#                                transmissivity,
#                                thickness_aquifer,
#                                height_water,
#                                radius_well,
#                                radius_well)
# points(Mod(response)~frequency, cooper, col = 'red', type = 'l')
#
# head(crsp)
# head(cooper$response)
# all.equal(crsp, cooper$response)
#
#
#
# hsieh <- tidal_hsieh_1987(frequency,
#                           storativity,
#                           transmissivity,
#                           radius_well)
# points(Mod(response)~frequency, hsieh, col = 'blue', type = 'l')
#
#
# plot(unwrap(Arg(response)) * 180/pi~frequency, cooper, col = 'red', type = 'l', log = 'x', ylim = c(0, -180))
# points(unwrap(Arg(response)) * 180/pi~frequency, hsieh, col = 'blue', type = 'l', log = 'x', ylim = c(0, -180))
# points(unwrap(Arg(-response)) * 180/pi~frequency, liu, col = 'black', type = 'l', log = 'x', ylim = c(0, -180))
#
#
# plot(unwrap(Arg(response)) * 180/pi~frequency, cooper, col = 'red', type = 'l', log = 'x', ylim = c(0, -180))
# points(unwrap(Arg(response)) * 180/pi~frequency, hsieh, col = 'blue', type = 'l', log = 'x', ylim = c(0, -180))
# plot(unwrap(Arg(response)) * 180/pi~frequency, liu, col = 'black', type = 'l', log = 'x')
#
#
