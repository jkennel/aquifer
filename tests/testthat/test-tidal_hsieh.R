test_that("hsieg amplitude works", {
  library(data.table)
  data('hsieh_1987_fig_2_3')
  gain <- hsieh_1987_fig_2_3[variable == 'gain']

  transmissivity <- 1e-03
  storativity    <- 1e-3
  gain_sub       <- gain[S == storativity]
  radius_well    <- 0.05
  frequency      <- (gain_sub$dimensionless_frequency * radius_well^2 / transmissivity)^(-1)

  hsieh <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)

  # plot(Mod(response)~dimensionless_frequency, hsieh, type='l', log = 'x')
  # points(response~dimensionless_frequency, gain_sub)

  d <- mean(abs((Mod(hsieh$response) - gain_sub$response)))

  expect_lt(d, 0.003)


  storativity  <- 1e-4
  gain_sub     <- gain[S == storativity]
  frequency    <- (gain_sub$dimensionless_frequency * radius_well^2 / transmissivity)^(-1)
  hsieh        <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)

  d <- mean(abs((Mod(hsieh$response) - gain_sub$response)))
  expect_lt(d, 0.003)


  storativity  <- 1e-5
  gain_sub     <- gain[S == storativity]
  frequency    <- (gain_sub$dimensionless_frequency * radius_well^2 / transmissivity)^(-1)
  hsieh        <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)

  d <- mean(abs((Mod(hsieh$response) - gain_sub$response)))
  expect_lt(d, 0.003)

  storativity  <- 1e-6
  gain_sub     <- gain[S == storativity]
  frequency    <- (gain_sub$dimensionless_frequency * radius_well^2 / transmissivity)^(-1)
  hsieh        <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)

  d <- mean(abs((Mod(hsieh$response) - gain_sub$response)))
  expect_lt(d, 0.003)

  storativity  <- 1e-7
  gain_sub     <- gain[S == storativity]
  frequency    <- (gain_sub$dimensionless_frequency * radius_well^2 / transmissivity)^(-1)
  hsieh        <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)

  d <- mean(abs((Mod(hsieh$response) - gain_sub$response)))
  expect_lt(d, 0.003)

})

test_that("hsieg phase works", {
  library(data.table)
  data('hsieh_1987_fig_2_3')
  phase <- hsieh_1987_fig_2_3[variable == 'phase']

  transmissivity <- 1e-03
  storativity    <- 1e-3
  phase_sub       <- phase[S == storativity]
  radius_well    <- 0.05
  frequency      <- (phase_sub$dimensionless_frequency * radius_well^2 / transmissivity)^(-1)

  hsieh <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)

  # plot(Arg(response)*180/pi~dimensionless_frequency, hsieh, type='l', log = 'x')
  # points(response~dimensionless_frequency, phase_sub)

  d <- mean(abs((Arg(hsieh$response)*180/pi - phase_sub$response)))
  expect_lt(d, 0.5)


  storativity  <- 1e-4
  phase_sub    <- phase[S == storativity]
  frequency    <- (phase_sub$dimensionless_frequency * radius_well^2 / transmissivity)^(-1)
  hsieh        <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)

  d <- mean(abs((Arg(hsieh$response)*180/pi - phase_sub$response)))
  expect_lt(d, 0.5)


  storativity  <- 1e-5
  phase_sub    <- phase[S == storativity]
  frequency    <- (phase_sub$dimensionless_frequency * radius_well^2 / transmissivity)^(-1)
  hsieh        <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)

  d <- mean(abs((Arg(hsieh$response)*180/pi - phase_sub$response)))
  expect_lt(d, 0.5)

  storativity  <- 1e-6
  phase_sub    <- phase[S == storativity]
  frequency    <- (phase_sub$dimensionless_frequency * radius_well^2 / transmissivity)^(-1)
  hsieh        <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)

  d <- mean(abs((Arg(hsieh$response)*180/pi - phase_sub$response)))
  expect_lt(d, 0.5)

  storativity  <- 1e-7
  phase_sub     <- phase[S == storativity]
  frequency    <- (phase_sub$dimensionless_frequency * radius_well^2 / transmissivity)^(-1)
  hsieh        <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)

  d <- mean(abs((Arg(hsieh$response)*180/pi - phase_sub$response)))
  expect_lt(d, 0.5)


})
