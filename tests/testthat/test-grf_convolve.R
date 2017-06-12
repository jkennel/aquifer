context("grf_convolve")

test_that("grf_convolve works", {
  radius <- 10
  storativity <- 10e-5
  transmissivity <- 10e-4
  flow_rate <- rep(0.01, 1000)
  flow_dimension <- 2
  time <- 1:1000

  expect_equal(grf_convolve(radius, storativity, transmissivity, 1, time,
                                 rep(0.01, 1000), flow_dimension),
               hantush_convolve(radius, storativity, transmissivity, 1e6, time,
                                 rep(0.01, 1000)))

  expect_equal(grf_convolve(radius, storativity, transmissivity, 1, time,
                              rep(0.00, 1000), flow_dimension),
               rep(0, 1000))
  })
