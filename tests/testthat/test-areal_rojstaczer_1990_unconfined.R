test_that("rojstaczer & riley 1990 areal unconfined amplitude works", {
  library(data.table)
  data('rojstaczer_1990_fig_4')

  thickness_aquifer    <- 100
  thickness_vadose     <- thickness_aquifer
  thickness_saturated_well <- thickness_aquifer
  diffusivity_vertical <- 0.01

  specific_yield       <- 0.02
  storage_aquifer      <- 0.001
  k_vertical           <- 0.0     # set ohm = 0

  transmissivity       <- 1e-4

  attenuation          <- 1.0
  loading_efficiency   <- 0.5


  gain <- rojstaczer_1990_fig_4[variable == 'gain']
  R_Qu_ratio <- 1000
  diffusivity_vadose   <- 0.00001

  gain_sub <- gain[R_div_Qu == R_Qu_ratio]
  frequency <- gain_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- areal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          transmissivity,
                                          storage_aquifer,
                                          specific_yield,
                                          k_vertical,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_saturated_well,
                                          thickness_vadose,
                                          thickness_aquifer,
                                          loading_efficiency,
                                          attenuation)

  plot(Mod(response)~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  points(response~Qu, gain_sub)
  d <- mean(abs((Mod(roj_1990$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.01)



  R_Qu_ratio <- 100
  diffusivity_vadose   <- 0.0001
  gain_sub <- gain[R_div_Qu == R_Qu_ratio]
  frequency <- gain_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- areal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          transmissivity,
                                          storage_aquifer,
                                          specific_yield,
                                          k_vertical,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_saturated_well,
                                          thickness_vadose,
                                          thickness_aquifer,
                                          loading_efficiency,
                                          attenuation)

  # plot(Mod(response)~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~Qu, gain_sub)
  d <- mean(abs((Mod(roj_1990$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.01)




  R_Qu_ratio <- 10
  diffusivity_vadose   <- 0.001
  gain_sub <- gain[R_div_Qu == R_Qu_ratio]
  frequency <- gain_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- areal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          transmissivity,
                                          storage_aquifer,
                                          specific_yield,
                                          k_vertical,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_saturated_well,
                                          thickness_vadose,
                                          thickness_aquifer,
                                          loading_efficiency,
                                          attenuation)

  # plot(Mod(response)~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~Qu, gain_sub)
  d <- mean(abs((Mod(roj_1990$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.01)



  R_Qu_ratio <- 1
  diffusivity_vadose   <- 0.01
  gain_sub <- gain[R_div_Qu == R_Qu_ratio]
  frequency <- gain_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- areal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          transmissivity,
                                          storage_aquifer,
                                          specific_yield,
                                          k_vertical,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_saturated_well,
                                          thickness_vadose,
                                          thickness_aquifer,
                                          loading_efficiency,
                                          attenuation)

  # plot(Mod(response)~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~Qu, gain_sub)
  d <- mean(abs((Mod(roj_1990$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.01)




  R_Qu_ratio <- 1e-4
  diffusivity_vadose   <- 10
  gain_sub <- gain[R_div_Qu == R_Qu_ratio]
  frequency <- gain_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- areal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          transmissivity,
                                          storage_aquifer,
                                          specific_yield,
                                          k_vertical,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_saturated_well,
                                          thickness_vadose,
                                          thickness_aquifer,
                                          loading_efficiency,
                                          attenuation)

  # plot(Mod(response)~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~Qu, gain_sub)
  d <- mean(abs((Mod(roj_1990$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.01)


})







test_that("rojstaczer & riley 1990 areal unconfined phase works", {
  library(data.table)
  data('rojstaczer_1990_fig_4')

  thickness_aquifer    <- 100
  thickness_vadose     <- thickness_aquifer
  thickness_saturated_well <- thickness_aquifer
  diffusivity_vertical <- 0.01

  specific_yield       <- 0.02
  storage_aquifer      <- 0.001
  k_vertical           <- 0.0     # set ohm = 0

  storativity          <- 1e-5
  transmissivity       <- 1e-4

  attenuation          <- 1.0
  loading_efficiency   <- 0.5


  phase <- rojstaczer_1990_fig_4[variable == 'phase']
  R_Qu_ratio <- 1000
  diffusivity_vadose   <- 0.00001

  phase_sub <- phase[R_div_Qu == R_Qu_ratio]
  frequency <- phase_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- areal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          transmissivity,
                                          storage_aquifer,
                                          specific_yield,
                                          k_vertical,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_saturated_well,
                                          thickness_vadose,
                                          thickness_aquifer,
                                          loading_efficiency,
                                          attenuation)

  # plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~Qu, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1990$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.5)



  R_Qu_ratio <- 100
  diffusivity_vadose   <- 0.0001
  phase_sub <- phase[R_div_Qu == R_Qu_ratio]
  frequency <- phase_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- areal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          transmissivity,
                                          storage_aquifer,
                                          specific_yield,
                                          k_vertical,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_saturated_well,
                                          thickness_vadose,
                                          thickness_aquifer,
                                          loading_efficiency,
                                          attenuation)

  # plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~Qu, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1990$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.5)




  R_Qu_ratio <- 10
  diffusivity_vadose   <- 0.001
  phase_sub <- phase[R_div_Qu == R_Qu_ratio]
  frequency <- phase_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- areal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          transmissivity,
                                          storage_aquifer,
                                          specific_yield,
                                          k_vertical,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_saturated_well,
                                          thickness_vadose,
                                          thickness_aquifer,
                                          loading_efficiency,
                                          attenuation)

  # plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~Qu, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1990$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.5)



  R_Qu_ratio <- 1
  diffusivity_vadose   <- 0.01
  phase_sub <- phase[R_div_Qu == R_Qu_ratio]
  frequency <- phase_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- areal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          transmissivity,
                                          storage_aquifer,
                                          specific_yield,
                                          k_vertical,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_saturated_well,
                                          thickness_vadose,
                                          thickness_aquifer,
                                          loading_efficiency,
                                          attenuation)

  # plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~Qu, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1990$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.5)




  R_Qu_ratio <- 1e-4
  diffusivity_vadose   <- 10
  phase_sub <- phase[R_div_Qu == R_Qu_ratio]
  frequency <- phase_sub$Qu * 2.0 * diffusivity_vertical / (2.0 * pi * thickness_saturated_well^2)

  roj_1990 <- areal_rojstaczer_unconfined(frequency,
                                          radius_well,
                                          transmissivity,
                                          storage_aquifer,
                                          specific_yield,
                                          k_vertical,
                                          diffusivity_vertical,
                                          diffusivity_vadose,
                                          thickness_saturated_well,
                                          thickness_vadose,
                                          thickness_aquifer,
                                          loading_efficiency,
                                          attenuation)

  # plot(unwrap(Arg(response)) * 180/pi~Qu, roj_1990, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~Qu, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1990$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.5)


})



# test_that("rojstaczer & Riley 1990 unconfined barometric and loading comparison", {
#
# library(data.table)
#
# thickness_aquifer    <- 100
# thickness_vadose     <- thickness_aquifer
# thickness_saturated_well <- thickness_aquifer
# diffusivity_vertical <- 0.01
#
# specific_yield       <- 0.02
# storage_aquifer      <- 0.001
# k_vertical           <- 0.0     # set ohm = 0
#
# storativity          <- 1e-5
# transmissivity       <- 1e-4
#
# attenuation          <- 1.0
# loading_efficiency   <- 0.5
#
#
# diffusivity_vadose   <- 0.00001
#
# frequency <- 10^seq(-3, 3, 0.1)
#
# roj_1990_b <- areal_rojstaczer_unconfined(frequency,
#                                         radius_well,
#                                         transmissivity,
#                                         storage_aquifer,
#                                         specific_yield,
#                                         k_vertical,
#                                         diffusivity_vertical,
#                                         diffusivity_vadose,
#                                         thickness_saturated_well,
#                                         thickness_vadose,
#                                         thickness_aquifer,
#                                         loading_efficiency,
#                                         attenuation)
#
# roj_1990_l <- areal_rojstaczer_unconfined(frequency,
#                                           radius_well,
#                                           transmissivity,
#                                           storage_aquifer,
#                                           specific_yield,
#                                           k_vertical,
#                                           diffusivity_vertical,
#                                           diffusivity_vadose,
#                                           thickness_saturated_well,
#                                           thickness_vadose,
#                                           thickness_aquifer,
#                                           loading_efficiency,
#                                           attenuation)
# expect_equal(Mod(roj_1990_l$response), 1 - Mod(roj_1990_b$response))
#
# })
