context("grf")

test_that("grf works", {
  storativity = 1e-5
  radius = 20
  Kf = 1e-3
  thickness <- 1
  times <- 1:2000
  flow_rates <- abs(rnorm(10)) / 1000
  flow_rate_times <- 0:9 * 200
  flow_dimension <- 2

  expect_equal(grf(radius,
      storativity,
      Kf,
      thickness,
      times,
      flow_rates,
      flow_rate_times,
      flow_dimension),grf_time_parallel(radius = radius,
                                        storativity = storativity,
                                        K = Kf,
                                        thickness = thickness,
                                        time = times,
                                        flow_rate = rep(flow_rates, each = 200),
                                        flow_time_interval = 1,
                                        flow_dimension = flow_dimension)
  )

})
