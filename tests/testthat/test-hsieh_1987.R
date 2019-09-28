test_that("Hsieh 1987 works", {

  library(data.table)
  library(Bessel)

  storativity    <- 1e-05
  transmissivity <- 1e-05
  radius_well    <- 0.05
  radius_casing  <- 0.05

  data("hsieh_gain")
  frequency <- transmissivity / (hsieh_gain$frequency * radius_casing^2)

  gain <- tidal_hsieh_1987(frequency,
                               storativity,
                               transmissivity,
                               radius_well)


  d <- range(abs(Mod(gain$response) - hsieh_gain$gain))
  expect_lt(max(abs(d)), 0.01)

  # frf <- tidal_hsieh_1987(frequency,
  #                         storativity,
  #                         transmissivity,
  #                         radius_well)
  #
  # # use_data(hsieh_gain, overwrite = TRUE)
  # plot(frequency, hsieh_gain$gain,log = 'x', ylim = c(0, 1))
  # points(Mod(response)~frequency, frf, col = 'red', type='l', lwd = 4)



  data("hsieh_phase")
  frequency <- transmissivity / (hsieh_phase$frequency * radius_casing^2)
  phase <- tidal_hsieh_1987(frequency,
                                       storativity,
                                       transmissivity,
                                       radius_well)

  d <- range(abs(unwrap(Arg(phase$response)) * 180 / pi - hsieh_phase$phase))
  expect_lt(max(abs(d)), 0.5)

  # frf <- tidal_hsieh_1987(frequency,
  #                         storativity,
  #                         transmissivity,
  #                         radius_well)
  #
  # # use_data(hsieh_gain, overwrite = TRUE)
  # plot(frequency, hsieh_phase$phase, log = 'x', ylim = c(0, -90))
  # points(unwrap(Arg(response)) * 180/pi~frequency, frf, col = 'red', type='l', lwd = 4)

})
