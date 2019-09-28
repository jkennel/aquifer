test_that("tidal rojstaczer 1988a gain works", {
  library(data.table)
  data('rojstaczer_1988a_fig_3')
  gain <- rojstaczer_1988a_fig_3[variable == 'gain']
  storativity <- 1e-4
  transmissivity <- 1e-4
  height_water <- 10
  frequency <- gain$dimensionless_frequency * 2.0 * transmissivity/storativity /
               (2.0 * pi * height_water^2)

  roj_1988 <- tidal_rojstaczer_1988(frequency, storativity, transmissivity, height_water)
  plot(0.05 * Mod(response)~Q, roj_1988, type='l', log =  'x')
  points(response~dimensionless_frequency, gain)

  d <- mean(abs((0.05 * Mod(roj_1988$response) - gain$response)))

  expect_lt(max(abs(d)), 0.0002)

})

test_that("tidal rojstaczer 1988a phase works", {
  library(data.table)
  data('rojstaczer_1988a_fig_3')
  storativity <- 1e-4
  transmissivity <- 1e-4
  height_water <- 10
  phase <- rojstaczer_1988a_fig_3[variable == 'phase']
  frequency <- phase$dimensionless_frequency * 2.0 * transmissivity/storativity /
    (2.0 * pi * height_water^2)

  roj_1988 <- tidal_rojstaczer_1988(frequency, storativity, transmissivity, height_water)
  plot(unwrap(Arg(response)) * 180/pi~Q, roj_1988, type='l', log =  'x')
  points(response~dimensionless_frequency, phase)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi - phase$response)))

  expect_lt(max(abs(d)), 0.1)


})
