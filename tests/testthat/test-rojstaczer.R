test_that("rojstaczer works", {

  data('hussein_phase')

  frequency <- hussein_phase$frequency

  rw  <- radius_well   <- 0.098
  transmissivity <- 20
  thickness_confining <- bcon   <- 14.8
  thickness_vadose <- lunsat <- 1.3
  diffusivity_confining <- diffusivity_vadose <- dcon   <- dunsat <- 35
  attenuation <- Tc     <- 1
  be     <- 0.6
  loading_efficiency <- 1 - be
  storage_confining <- Scon   <- 1e-3
  storage_aquifer <- Saqu   <- 1e-6

  x1 <- rojstaczer_r(frequency,
                  radius_well,
                  transmissivity,
                  storage_confining,
                  storage_aquifer,
                  diffusivity_confining,
                  diffusivity_vadose,
                  thickness_confining,
                  thickness_vadose,
                  loading_efficiency,
                  attenuation,
                  inverse = TRUE)

  phase <- (unwrap(Arg(x1)) * 180/pi)

  d <- mean(abs(phase - hussein_phase$phase))

  # plot(phase~frequency, hussein_phase, log = 'x')
  # points(phase~frequency, type='l', col = 'red', log = 'x')

  expect_lt(d, 1)



  data("hussein_gain")

  frequency <- hussein_gain$frequency
  #use_data(hussein_gain)

  x1 <- rojstaczer_r(frequency,
                   radius_well,
                   transmissivity,
                   storage_confining,
                   storage_aquifer,
                   diffusivity_confining,
                   diffusivity_vadose,
                   thickness_confining,
                   thickness_vadose,
                   loading_efficiency,
                   attenuation,
                   inverse = TRUE)

  # plot(1-gain~frequency, hussein_gain, log = 'x', ylim = c(0, 1))
  # points(Mod(x1)~frequency, type='p', col = 'red')
  # plot(Mod(x1)~frequency, type='l', col = 'red', log = 'x')
  # abline(h = 0.5)
  #
  # phase <- Arg(x1) * 180/pi #-180 #(atan2(Im(x1), Re(x1)) * 180/pi)
  # phase <- ifelse(phase > 0, phase-360, phase)

  # plot(phase~frequency, hussein_phase, log = 'x', ylim = c(-280, -100))
  # plot(phase~hussein_gain$frequency, type='l', col = 'red', log = 'x')
  # abline(h = -180)


  # phase <- (atan2(Im(x1), Re(x1)) * 180/pi)
  # phase <- ifelse(phase < 0, phase + 360, phase)
  # phase <- phase - 180
  # plot(phase~frequency, hussein_phase, log = 'x')
  # plot(phase~(hussein_gain$frequency), type='l', col = 'red', log = 'x')




  d <- mean(abs(Mod(x1) - hussein_gain$gain))
  # plot(gain~frequency, hussein_gain, log = 'x', ylim = c(0, 1))
  # points(Mod(x1)~frequency, type='l', col = 'red', log = 'x')

  # tan_theta <- -Im(x1)/Re(x1)
  #
  # x2 <- sqrt((1-Mod(x1))^2 / (1 + tan_theta^2))
  # y2 <- x2 * tan_theta
  # a <- complex(real = x2, imaginary = y2)
  # plot(Mod(a))

  expect_lt(d, 0.005)



  # plot(1-gain~frequency, hussein_gain, log = 'x', ylim = c(0, 1))
  # points(Mod(1/x1)~frequency, type='l', col = 'red', log = 'x')
  #
  # 1-hussein_gain$gain[1]
  #
  # x1[1]
  # Mod(1-x1[1])
  # 1-Mod(x1[1])
  # Mod(((complex(real = -1, imaginary = 0) + (x1[1]))))

  expect_equal(as.vector(rojstaczer_parallel(frequency,
                                radius_well,
                                transmissivity,
                                storage_confining,
                                storage_aquifer,
                                diffusivity_confining,
                                diffusivity_vadose,
                                thickness_confining,
                                thickness_vadose,
                                loading_efficiency,
                                attenuation)),
            rojstaczer_r(frequency,
                         radius_well,
                         transmissivity,
                         storage_confining,
                         storage_aquifer,
                         diffusivity_confining,
                         diffusivity_vadose,
                         thickness_confining,
                         thickness_vadose,
                         loading_efficiency,
                         attenuation))



# frequency <- 10^seq(-0, 6, length.out = 400)
#
#   microbenchmark(
#     x1c <- rojstaczer_parallel(frequency,
#                      radius_well,
#                      transmissivity,
#                      storage_confining,
#                      storage_aquifer,
#                      diffusivity_confining,
#                      diffusivity_vadose,
#                      thickness_confining,
#                      thickness_vadose,
#                      loading_efficiency,
#                      attenuation),
#     x1r <- rojstaczer_r(frequency,
#                      radius_well,
#                      transmissivity,
#                      storage_confining,
#                      storage_aquifer,
#                      diffusivity_confining,
#                      diffusivity_vadose,
#                      thickness_confining,
#                      thickness_vadose,
#                      loading_efficiency,
#                      attenuation),
#     times = 10
#   )
# # # head(x1c)
# # # head(x1r)
# # #
# # # tail(x1c)
# # # tail(x1r)
# all.equal(as.vector(x1c), x1r)

})
