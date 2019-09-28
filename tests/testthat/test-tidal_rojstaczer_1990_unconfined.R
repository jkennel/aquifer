
test_that("rojstaczer & riley 1990 unconfined ohm works", {
  library(data.table)

  specific_yield       <- 0.02
  storage_aquifer      <- 0.001
  k_vertical           <- 0.001     # set ohm = 0


  data('rojstaczer_1990_fig_2')
  sol <- copy(rojstaczer_1990_fig_2)
  frequency <- 1/(2 * pi * sol$ohm^2 / (storage_aquifer * k_vertical) * (2 * specific_yield ^2))

  omega <- .calc_omega(frequency)
  ohm   <- .calc_ohm(omega, storage_aquifer, k_vertical, specific_yield)

  p0 <- 1/(1-ohm)-1

  plot(response~ohm, sol, log='x')
  points(Re(ohm / (1 - 1i)), Mod(p0), type='l')


  d <- mean(abs((Mod(p0) - sol$response)))

  expect_lt(max(abs(d)), 0.03)


})



test_that("rojstaczer & riley 1990 unconfined amplitude works", {

  library(data.table)

  thickness_aquifer    <- 100
  thickness_vadose     <- thickness_aquifer * 5
  diffusivity_vertical <- 0.01
  diffusivity_vadose   <- 0.01

  specific_yield       <- 0.02
  storage_aquifer      <- 0.001
  k_vertical           <- 0     # set ohm = 0

  storativity          <- 1e-5
  transmissivity       <- 1e-4

  data('rojstaczer_1990_fig_3')
  gain <- rojstaczer_1990_fig_3[variable == 'gain']
  thickness_saturated_well <- thickness_aquifer
  b_d_ratio <- thickness_saturated_well / thickness_aquifer
  gain_sub <- gain[b_div_d == b_d_ratio]
  frequency <- gain_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- tidal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          k_vertical,
                                          storage_aquifer,
                                          specific_yield,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_aquifer,
                                          thickness_vadose,
                                          thickness_saturated_well)
  plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2), ylim = c(0, 0.5))
  points(response~Qu, gain_sub)

  d <- mean(abs((0.5*Mod(roj_1990$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.001)




  thickness_saturated_well <- thickness_aquifer * 0.5
  b_d_ratio <- thickness_saturated_well / thickness_aquifer
  gain_sub <- gain[b_div_d == b_d_ratio]
  frequency <- gain_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- tidal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          k_vertical,
                                          storage_aquifer,
                                          specific_yield,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_aquifer,
                                          thickness_vadose,
                                          thickness_saturated_well)
  plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2), ylim = c(0, 0.5))
  points(response~Qu, gain_sub)

  d <- mean(abs((0.5*Mod(roj_1990$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.001)


  thickness_saturated_well <- thickness_aquifer * 0.25
  b_d_ratio <- thickness_saturated_well / thickness_aquifer
  gain_sub <- gain[b_div_d == b_d_ratio]
  frequency <- gain_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- tidal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          k_vertical,
                                          storage_aquifer,
                                          specific_yield,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_aquifer,
                                          thickness_vadose,
                                          thickness_saturated_well)
  plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2), ylim = c(0, 0.5))
  points(response~Qu, gain_sub)

  d <- mean(abs((0.5*Mod(roj_1990$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.001)


  thickness_saturated_well <- thickness_aquifer * 0.01
  b_d_ratio <- thickness_saturated_well / thickness_aquifer
  gain_sub <- gain[b_div_d == b_d_ratio]
  frequency <- gain_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- tidal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          k_vertical,
                                          storage_aquifer,
                                          specific_yield,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_aquifer,
                                          thickness_vadose,
                                          thickness_saturated_well)
  plot(0.5 * Mod(response)~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2), ylim = c(0, 0.5))
  points(response~Qu, gain_sub)

  d <- mean(abs((0.5*Mod(roj_1990$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.001)


})



test_that("rojstaczer & riley 1990 unconfined phase works", {

  library(data.table)

  thickness_aquifer    <- 100
  thickness_vadose     <- thickness_aquifer * 5
  diffusivity_vertical <- 0.01
  diffusivity_vadose   <- 0.01

  specific_yield       <- 0.02
  storage_aquifer      <- 0.001
  k_vertical           <- 0     # set ohm = 0

  storativity          <- 1e-5
  transmissivity       <- 1e-4



  data('rojstaczer_1990_fig_3')
  phase <- rojstaczer_1990_fig_3[variable == 'phase']
  thickness_saturated_well <- thickness_aquifer
  b_d_ratio <- thickness_saturated_well / thickness_aquifer
  phase_sub <- phase[b_div_d == b_d_ratio]
  frequency <- phase_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- tidal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          k_vertical,
                                          storage_aquifer,
                                          specific_yield,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_aquifer,
                                          thickness_vadose,
                                          thickness_saturated_well)
  plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  points(response~Qu, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1990$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)




  thickness_saturated_well <- thickness_aquifer * 0.5
  b_d_ratio <- thickness_saturated_well / thickness_aquifer
  phase_sub <- phase[b_div_d == b_d_ratio]
  frequency <- phase_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- tidal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          k_vertical,
                                          storage_aquifer,
                                          specific_yield,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_aquifer,
                                          thickness_vadose,
                                          thickness_saturated_well)
  plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  points(response~Qu, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1990$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)


  thickness_saturated_well <- thickness_aquifer * 0.25
  b_d_ratio <- thickness_saturated_well / thickness_aquifer
  phase_sub <- phase[b_div_d == b_d_ratio]
  frequency <- phase_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- tidal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          k_vertical,
                                          storage_aquifer,
                                          specific_yield,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_aquifer,
                                          thickness_vadose,
                                          thickness_saturated_well)
  plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  points(response~Qu, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1990$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)


  thickness_saturated_well <- thickness_aquifer * 0.01
  b_d_ratio <- thickness_saturated_well / thickness_aquifer
  phase_sub <- phase[b_div_d == b_d_ratio]
  frequency <- phase_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- tidal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          k_vertical,
                                          storage_aquifer,
                                          specific_yield,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_aquifer,
                                          thickness_vadose,
                                          thickness_saturated_well)
  plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  points(response~Qu, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1990$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)


})

