context("hantush_time_parallel")

test_that("hantush_time_parallel works", {
  radius <- 10
  storativity <- 10e-5
  K <- 10e-4
  thickness <- 1
  transmissivity <- K*thickness
  leakage <- 300
  flow_rate <- rep(0.01, 1000)
  flow_dimension <- 2
  time <- 1:1000


  expect_equal(hantush_time_parallel(radius, storativity, transmissivity, leakage, time,
                                 rep(0.01, 1000), 1, 10),
               hantush_time_parallel(radius, storativity, transmissivity, leakage, time,
                                 rep(0.01, 100), 10, 10))

  expect_equal(hantush_time_parallel(radius, storativity, transmissivity, 1e6, time,
                                     rep(0.01, 1000), 1, 10),
               grf_time_parallel(radius, storativity, K, thickness, time,
                                 rep(0.01, 100), 10, flow_dimension))

})
