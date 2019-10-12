test_that("rojstaczer 1988 areal amplitude figure 3 works", {

  library(data.table)
  data("rojstaczer_1988b_fig_3")

  thickness_confining  <- 10
  thickness_vadose     <- 1
  diffusivity_confining <- 0.05

  storage_aquifer      <- 1e-4
  storage_confining    <- 1e-4

  attenuation          <- 1.0
  loading_efficiency   <- 0.5

  radius_well          <- 0.10
  diffusivity_vadose   <- 0.1

  gain <- rojstaczer_1988b_fig_3[variable == 'gain']
  Q_W_ratio <- 10000
  transmissivity       <- 1e-1

  gain_sub  <- gain[Q_div_W == Q_W_ratio]
  frequency <- gain_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  #plot(Mod(response)~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 10), ylim = c(0, 1))
  plot(unwrap(Arg(response)) * 180/pi~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 10), ylim = c(-360, 360))

  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
                                            inverse = FALSE)
  #points(Mod(response)~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 10), col = 'red')
  roj_1988$response <- complex(real = Re(roj_1988$response),
                               im = Im(roj_1988$response))
  points(unwrap(Arg(response)) * 180/pi~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 10), col = 'red')



  # points(response~W, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.003)



  Q_W_ratio <- 1000
  transmissivity       <- 1e-2

  gain_sub  <- gain[Q_div_W == Q_W_ratio]
  frequency <- gain_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  # plot(Mod(response)~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 10))
  # points(response~W, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.003)




  Q_W_ratio <- 100
  transmissivity       <- 1e-3

  gain_sub  <- gain[Q_div_W == Q_W_ratio]
  frequency <- gain_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  # plot(Mod(response)~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 10))
  # points(response~W, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.003)




  Q_W_ratio <- 10
  transmissivity       <- 1e-4

  gain_sub  <- gain[Q_div_W == Q_W_ratio]
  frequency <- gain_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  # plot(Mod(response)~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 10))
  # points(response~W, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.003)




  Q_W_ratio <- 1
  transmissivity       <- 1e-5

  gain_sub  <- gain[Q_div_W == Q_W_ratio]
  frequency <- gain_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  # plot(Mod(response)~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 10))
  # points(response~W, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.003)


})


test_that("rojstaczer 1988 areal phase figure 3 works", {

  library(data.table)
  data("rojstaczer_1988b_fig_3")

  thickness_confining  <- 10
  thickness_vadose     <- 1
  diffusivity_confining <- 0.05

  storage_aquifer      <- 1e-4
  storage_confining    <- 1e-4

  attenuation          <- 1.0
  loading_efficiency   <- 0.5

  radius_well          <- 0.10
  diffusivity_vadose   <- 0.1

  phase <- rojstaczer_1988b_fig_3[variable == 'phase']
  Q_W_ratio <- 10000
  transmissivity       <- 1e-1

  phase_sub  <- phase[Q_div_W == Q_W_ratio]
  frequency <- phase_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  # plot(unwrap(Arg(response)) * 180/pi~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 1e1))
  # points(response~W, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)



  Q_W_ratio <- 1000
  transmissivity       <- 1e-2

  phase_sub  <- phase[Q_div_W == Q_W_ratio]
  frequency <- phase_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  # plot(unwrap(Arg(response)) * 180/pi~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 1e1))
  # points(response~W, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)




  Q_W_ratio <- 100
  transmissivity       <- 1e-3

  phase_sub  <- phase[Q_div_W == Q_W_ratio]
  frequency <- phase_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  # plot(unwrap(Arg(response)) * 180/pi~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 1e1))
  # points(response~W, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)




  Q_W_ratio <- 10
  transmissivity       <- 1e-4

  phase_sub  <- phase[Q_div_W == Q_W_ratio]
  frequency <- phase_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  # plot(unwrap(Arg(response)) * 180/pi~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 1e1))
  # points(response~W, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)



  Q_W_ratio <- 1
  transmissivity       <- 1e-5

  phase_sub  <- phase[Q_div_W == Q_W_ratio]
  frequency <- phase_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  # plot(unwrap(Arg(response)) * 180/pi~W, roj_1988, type='l', log = 'x', xlim = c(1e-4, 1e1))
  # points(response~W, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)



})


test_that("rojstaczer 1988 areal amplitude figure 5 works", {


  library(data.table)
  data("rojstaczer_1988b_fig_5")

  thickness_confining  <- 100
  thickness_vadose     <- 3
  diffusivity_confining <- 0.005

  transmissivity       <- 1e-1

  attenuation          <- 1.0
  loading_efficiency   <- 0.5

  radius_well          <- 0.10
  diffusivity_vadose   <- 0.1

  gain <- rojstaczer_1988b_fig_5[variable == 'gain']

  storage_aquifer      <- 1e-7
  storage_confining    <- 1e-7
  gain_sub  <- gain[S == storage_confining]
  frequency <- gain_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(Mod(response)~W, roj_1988, type='l', log = 'x', xlim = c(1e-3, 10))
  # points(response~W, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.001)



  storage_confining    <- 1e-5
  storage_aquifer    <- 1e-5

  gain_sub  <- gain[S == storage_confining]
  frequency <- gain_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(Mod(response)~W, roj_1988, type='l', log = 'x', xlim = c(1e-3, 10))
  # points(response~W, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.001)




  storage_confining    <- 1e-3
  storage_aquifer    <- 1e-3

  gain_sub  <- gain[S == storage_confining]
  frequency <- gain_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(Mod(response)~W, roj_1988, type='l', log = 'x', xlim = c(1e-3, 10))
  # points(response~W, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.001)

})



test_that("rojstaczer 1988 areal phase figure 5 works", {
  # Had to add a 360 degree shift to the phase.
  # If the entire response from low frequency to high frequency is calculated
  # this shift is unnessary as unwrap does a good job of handling this.

  library(data.table)
  data("rojstaczer_1988b_fig_5")

  thickness_confining  <- 100
  thickness_vadose     <- 3
  diffusivity_confining <- 0.005

  transmissivity       <- 1e-1

  attenuation          <- 1.0
  loading_efficiency   <- 0.5

  radius_well          <- 0.10
  diffusivity_vadose   <- 0.1

  phase <- rojstaczer_1988b_fig_5[variable == 'phase']

  storage_aquifer      <- 1e-7
  storage_confining    <- 1e-7
  phase_sub  <- phase[S == storage_confining]
  frequency <- phase_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  # had to add a 360 degree shift
  # plot((Arg(response)) * 180/pi-360~W, roj_1988, type='l', log = 'x', xlim = c(1e-3, 1e1))
  # points(response~W, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi -360 - phase_sub$response)))

  expect_lt(max(abs(d)), 0.2)



  storage_confining <- 1e-5
  storage_aquifer   <- 1e-5

  phase_sub <- phase[S == storage_confining]
  frequency <- phase_sub$W * transmissivity / (2.0 * pi * radius_well^2)

  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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
  # had to add a 360 degree shift
  # plot(unwrap(Arg(response)) * 180/pi-360~W, roj_1988, type='l', log = 'x')
  # points(response~W, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi -360 - phase_sub$response)))

  expect_lt(max(abs(d)), 0.2)




  storage_confining    <- 1e-3
  storage_aquifer    <- 1e-3

  phase_sub  <- phase[S == storage_confining]
  frequency <- phase_sub$W * transmissivity / (2.0 * pi * radius_well^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # had to add a 360 degree shift
  # plot((Arg(response)) * 180/pi-360~W, roj_1988, type='l', log = 'x', xlim = c(1e-3, 1e1))
  # points(response~W, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi -360 - phase_sub$response)))

  expect_lt(max(abs(d)), 0.2)

})



test_that("rojstaczer 1988 areal amplitude figure 6 works", {

  library(data.table)
  data("rojstaczer_1988b_fig_6")

  thickness_confining  <- 10
  thickness_vadose     <- 100
  diffusivity_confining <- 0.001

  transmissivity       <- 1e-1

  attenuation          <- 1.0
  loading_efficiency   <- 0.5

  radius_well          <- 0.10
  storage_aquifer      <- 1e-7
  storage_confining    <- 1e-7

  gain <- rojstaczer_1988b_fig_6[variable == 'gain']
  diffusivity_vadose   <- 0.0001

  gain_sub  <- gain[R_div_Q == 1000]
  frequency <- gain_sub$dimensionless_frequency * 2.0 * diffusivity_confining / (2.0 * pi * thickness_confining^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(Mod(response)~Q, roj_1988, type='l', log = 'x', xlim = c(1e-3, 100), ylim = c(0, 1.5))
  # points(response~dimensionless_frequency, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.002)



  diffusivity_vadose   <- 0.001
  gain_sub  <- gain[R_div_Q == 100]
  frequency <- gain_sub$dimensionless_frequency * 2.0 * diffusivity_confining / (2.0 * pi * thickness_confining^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(Mod(response)~Q, roj_1988, type='l', log = 'x', xlim = c(1e-3, 100), ylim = c(0, 1.5))
  # points(response~dimensionless_frequency, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.002)



  diffusivity_vadose   <- 0.01
  gain_sub  <- gain[R_div_Q == 10]
  frequency <- gain_sub$dimensionless_frequency * 2.0 * diffusivity_confining / (2.0 * pi * thickness_confining^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(Mod(response)~Q, roj_1988, type='l', log = 'x', xlim = c(1e-3, 100), ylim = c(0, 1.5))
  # points(response~dimensionless_frequency, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.002)





  diffusivity_vadose   <- 0.1
  gain_sub  <- gain[R_div_Q == 1]
  frequency <- gain_sub$dimensionless_frequency * 2.0 * diffusivity_confining / (2.0 * pi * thickness_confining^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(Mod(response)~Q, roj_1988, type='l', log = 'x', xlim = c(1e-3, 100), ylim = c(0, 1.5))
  # points(response~dimensionless_frequency, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.002)


  diffusivity_vadose   <- 100
  gain_sub  <- gain[R_div_Q == 0.0001]
  frequency <- gain_sub$dimensionless_frequency * 2.0 * diffusivity_confining / (2.0 * pi * thickness_confining^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(Mod(response)~Q, roj_1988, type='l', log = 'x', xlim = c(1e-3, 100), ylim = c(0, 1.5))
  # points(response~dimensionless_frequency, gain_sub)
  d <- mean(abs((Mod(roj_1988$response) - gain_sub$response)))

  expect_lt(max(abs(d)), 0.002)



})


test_that("rojstaczer 1988 areal phase figure 6 works", {

  library(data.table)
  data("rojstaczer_1988b_fig_6")

  thickness_confining  <- 10
  thickness_vadose     <- 100
  diffusivity_confining <- 0.001

  transmissivity       <- 1e-1

  attenuation          <- 1.0
  loading_efficiency   <- 0.5

  radius_well          <- 0.10
  storage_aquifer      <- 1e-7
  storage_confining    <- 1e-7

  phase <- rojstaczer_1988b_fig_6[variable == 'phase']
  diffusivity_vadose   <- 0.0001

  phase_sub  <- phase[R_div_Q == 1000]
  frequency <- phase_sub$dimensionless_frequency * 2.0 * diffusivity_confining / (2.0 * pi * thickness_confining^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(unwrap(Arg(response)) * 180/pi~Q, roj_1988, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~dimensionless_frequency, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)



  diffusivity_vadose   <- 0.001
  phase_sub  <- phase[R_div_Q == 100]
  frequency <- phase_sub$dimensionless_frequency * 2.0 * diffusivity_confining / (2.0 * pi * thickness_confining^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(unwrap(Arg(response)) * 180/pi~Q, roj_1988, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~dimensionless_frequency, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)



  diffusivity_vadose   <- 0.01
  phase_sub  <- phase[R_div_Q == 10]
  frequency <- phase_sub$dimensionless_frequency * 2.0 * diffusivity_confining / (2.0 * pi * thickness_confining^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(unwrap(Arg(response)) * 180/pi~Q, roj_1988, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~dimensionless_frequency, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)





  diffusivity_vadose   <- 0.1
  phase_sub  <- phase[R_div_Q == 1]
  frequency <- phase_sub$dimensionless_frequency * 2.0 * diffusivity_confining / (2.0 * pi * thickness_confining^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(unwrap(Arg(response)) * 180/pi~Q, roj_1988, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~dimensionless_frequency, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)


  diffusivity_vadose   <- 100
  phase_sub  <- phase[R_div_Q == 0.0001]
  frequency <- phase_sub$dimensionless_frequency * 2.0 * diffusivity_confining / (2.0 * pi * thickness_confining^2)


  roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

  # plot(unwrap(Arg(response)) * 180/pi~Q, roj_1988, type='l', log = 'x', xlim = c(1e-3, 1e2))
  # points(response~dimensionless_frequency, phase_sub)

  d <- mean(abs((unwrap(Arg(roj_1988$response)) * 180/pi - phase_sub$response)))

  expect_lt(max(abs(d)), 0.3)





})



test_that("rojstaczer 1988 barometric and loading comparison", {

  library(data.table)

  thickness_confining  <- 10
  thickness_vadose     <- 100
  diffusivity_confining <- 0.001

  transmissivity       <- 1e-1

  attenuation          <- 1.0
  loading_efficiency   <- 0.5

  radius_well          <- 0.10
  storage_aquifer      <- 1e-5
  storage_confining    <- 1e-5

  diffusivity_vadose   <- 0.0001

  frequency <- 10^seq(-3, 1, 0.1)


  roj_1988_b <- areal_rojstaczer_semiconfined(frequency,
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

  roj_1988_l <- areal_rojstaczer_semiconfined(frequency,
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
                                            inverse = FALSE)


  expect_equal(Mod(roj_1988_l$response), 1-Mod(roj_1988_b$response))



})






thickness_confining  <- 1
thickness_vadose     <- 40
diffusivity_confining <- 100000

storage_aquifer      <- 1e-5
storage_confining    <- 1e-5

attenuation          <- 0.5
loading_efficiency   <- 0.5

radius_well          <- 0.1
diffusivity_vadose   <- 0.1

frequency <- 10^seq(-1, 5, 0.1)
transmissivity <- 5e-4
roj_1988 <- areal_rojstaczer_semiconfined(frequency,
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

library(ggplot2)
p <- ggplot(roj_1988, aes(x = Re(response), y = Im(response), colour = frequency))
p <- p + geom_point()
p <- p + scale_color_continuous()
p <- p + coord_equal()
p

plot(na.omit(roj_1988$response))
plot(na.omit(roj_1988$response))

plot(Mod(response)~frequency, roj_1988, type='l', log = 'x', ylim = c(0, 1))
