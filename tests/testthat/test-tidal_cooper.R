test_that("cooper amplitude works", {
  library(data.table)
  data('liu_1989_fig_8')
  thickness_aquifer <- 10
  amp_dat           <- liu_1989_fig_8[aquifer_thickness == thickness_aquifer & method == 'cooper']
  tau               <- amp_dat$period
  frequency         <- 1.0 / tau
  transmissivity    <- 2.19024
  storativity       <- 1e-4
  height_water      <- 100
  radius_well       <- 0.117
  cooper    <- tidal_cooper_1965(frequency,
                           storativity,
                           transmissivity,
                           thickness_aquifer,
                           height_water,
                           radius_well,
                           radius_well)

  # plot(Mod(response)~tau, cooper, type = 'l')
  # points(10^response~period,
  #        amp_dat,
  #        type='p', pch = 20, cex = 0.4)

  d <- mean(abs((Mod(cooper$response) - 10^amp_dat$response)))

  expect_lt(d, 2)


  thickness_aquifer <- 50
  amp_dat           <- liu_1989_fig_8[aquifer_thickness == thickness_aquifer & method == 'cooper']
  tau               <- amp_dat$period
  frequency         <- 1.0 / tau
  cooper    <- tidal_cooper_1965(frequency,
                           storativity,
                           transmissivity,
                           thickness_aquifer,
                           height_water,
                           radius_well,
                           radius_well)
  d <- mean(abs((Mod(cooper$response) - 10^amp_dat$response)))

  expect_lt(d, 2)


  thickness_aquifer <- 100
  amp_dat           <- liu_1989_fig_8[aquifer_thickness == thickness_aquifer & method == 'cooper']
  tau               <- amp_dat$period
  frequency         <- 1.0 / tau
  cooper    <- tidal_cooper_1965(frequency,
                           storativity,
                           transmissivity,
                           thickness_aquifer,
                           height_water,
                           radius_well,
                           radius_well)
  d <- mean(abs((Mod(cooper$response) - 10^amp_dat$response)))

  expect_lt(d, 2.2)


  thickness_aquifer <- 500
  amp_dat           <- liu_1989_fig_8[aquifer_thickness == thickness_aquifer & method == 'cooper']
  tau               <- amp_dat$period
  frequency         <- 1.0 / tau
  cooper    <- tidal_cooper_1965(frequency,
                           storativity,
                           transmissivity,
                           thickness_aquifer,
                           height_water,
                           radius_well,
                           radius_well)
  d <- mean(abs((Mod(cooper$response) - 10^amp_dat$response)))

  expect_lt(d, 4)

})
