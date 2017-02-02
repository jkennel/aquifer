context("grf_time_parallel")

test_that("grf_time_parallel works", {
  radius <- 10
  storativity <- 10e-5
  transmissivity <- 10e-4
  flow_rate <- rep(0.01, 1000)
  flow_dimension <- 2
  time <- 1:1000

  expect_equal(grf_time_parallel(radius, storativity, transmissivity, time,
                                 rep(0.01, 1000), 1, flow_dimension),
               grf_time_parallel(radius, storativity, transmissivity, time,
                                 rep(0.01, 100), 10, flow_dimension))
})
