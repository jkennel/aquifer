context("grf_time_parallel")

test_that("grf_time_parallel works", {
  radius <- 10
  storativity <- 10e-5
  K <- 10e-4
  thickness <- 1
  transmissivity <- K*thickness
  flow_rate <- rep(0.01, 1000)
  flow_dimension <- 2
  time <- 1:1000

  # flow_time_interval <- 100
  # flow_rate <- rep(0.01, length(time)/flow_time_interval)
  # coefs = grf_coefficient(flow_rate, radius, K, thickness, flow_dimension)
  #
  # v = 1 - flow_dimension/2
  # u = theis_u_time(radius, storativity, K, time)
  # u = grf_parallel(u, v)
  # u = impulse_function(u, flow_time_interval)
  # plot(u, type='l')
  # abline(v=flow_time_interval, lty=2, col='grey')
  # s = well_function_convolve(flow_time_interval, coefs, u);
  # plot(s, type='l')

  # check flow dimensions
  expect_equal(grf_time_parallel(radius, storativity, K, thickness, time,
                                 rep(0.01, 1000), 1, 1),
               grf_time_parallel(radius, storativity, K, thickness, time,
                                 rep(0.01, 100), 10, 1))

  expect_equal(grf_time_parallel(radius, storativity, K, thickness, time,
                                 rep(0.01, 1000), 1, flow_dimension),
               grf_time_parallel(radius, storativity, K, thickness, time,
                                 rep(0.01, 100), 10, flow_dimension))

  expect_equal(grf_time_parallel(radius, storativity, K, thickness, time,
                                 rep(0.01, 1000), 1, 3),
               grf_time_parallel(radius, storativity, K, thickness, time,
                                 rep(0.01, 100), 10, 3))

  # test flow_time_interval < 1 should equal the same as 1
  expect_warning(val <- grf_time_parallel(radius, storativity, K, thickness, time,
                                 rep(0.01, 1000), 0.5, 3))
  expect_equal(grf_time_parallel(radius, storativity, K, thickness, time,
                                 rep(0.01, 1000), 1, 3), val)


  # test flow_time_interval < 1 should equal the same as 1
  expect_warning(val <- grf_time_parallel(radius, storativity, K, thickness, time,
                                          rep(0.01, 1000), 2000, 3))
  expect_equal(grf_time_parallel(radius, storativity, K, thickness, time,
                                 rep(0.01, 1000), 1000, 3), val)

})
