context("hantush_convolve")


test_that("hantush_convolve works", {
  radius <- 10
  storativity <- 10e-5
  transmissivity <- 10e-4
  flow_rate <- rep(0.01, 1000)
  flow_dimension <- 2
  time <- 1:1000
  leakage <- 1e6

  expect_equal(hantush_convolve(radius, storativity, transmissivity, leakage, time,
                                rep(0.00, 1000)),
               rep(0, 1000))
})
