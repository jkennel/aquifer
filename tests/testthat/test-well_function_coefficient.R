context("well coefficients")

test_that("well_function_coefficient works", {
  expect_equal(well_function_coefficient(1e6, 1e-4), 1e6 / (4 * pi * 1e-4))
  expect_equal(well_function_coefficient(1, 1e-9), 1 / (4 * pi * 1e-9))
  expect_equal(well_function_coefficient(0, 1e-4), 0.0)
  expect_equal(well_function_coefficient(1e-4,0), Inf)
})

