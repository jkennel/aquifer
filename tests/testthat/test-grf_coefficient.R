context("grf coefficient")


test_that("grf_coefficient works", {
  expect_equal(grf_coefficient(0.0,10,1e-4,10,2), well_function_coefficient(0.0,1e-4*10))
  expect_equal(grf_coefficient(0.2,10,1e-4,10,2), well_function_coefficient(0.2,1e-4*10))
  expect_equal(grf_coefficient(0.0,10,0,10,2), NaN)
  expect_equal(grf_coefficient(c(0.0,0.0),10,1,10,2), c(0, 0))
  expect_equal(grf_coefficient(2,10,1,0,2), Inf)
  expect_equal(grf_coefficient(2,10,0,2,2), Inf)

  expect_equal(grf_coefficient(1,10,1e-4,10,1), (1 * 10^(2 * 0.5)) /
                 (4 * pi^(1 - 0.5) * 1e-4 * 10^(3 - 1)))
  expect_equal(grf_coefficient(1,10,1e-4,10,3), (1 * 10^(2 * -0.5)) /
                 (4 * pi^(1 - -0.5) * 1e-4 * 10^(3 - 3)))
})


# Rcpp::NumericVector flow_rate,
# double radius,
# double K,
# double thickness,
# double flow_dimension
