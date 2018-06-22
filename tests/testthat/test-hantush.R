context("hantush")

test_that("hantush works", {
  storativity = 1e-5
  radius = 20
  transmissivity = 1e-3
  leakage <- 100
  times <- 1:2000
  flow_rates <- abs(rnorm(10)) / 1000
  flow_rate_times <- 0:9 * 200
  n_terms <- 20

  expect_equal(hantush(radius,
                       storativity,
                       transmissivity,
                       leakage,
                       times,
                       flow_rates,
                       flow_rate_times,
                       n_terms),
               hantush_time_parallel(radius = radius,
                                     storativity = storativity,
                                     transmissivity = transmissivity,
                                     leakage = leakage,
                                     time = times,
                                     flow_rate = rep(flow_rates, each = 200),
                                     flow_time_interval = 1,
                                     n_terms = 20))
})
