context("exp_int_single")

## TODO: Rename context
## TODO: Add more tests

test_that("exp_int_single works", {
  expect_equal(exp_int_single(1, 0), 0.2193839, tolerance=1e-5)
  expect_equal(exp_int_single(1e6, 0), 0)
  expect_equal(exp_int_single(1e-6, 0), 13.2383, tolerance=1e-5)
  expect_equal(exp_int_single(1e-4, 0), 8.633225, tolerance=1e-5)
  expect_equal(exp_int_single(0, 0), Inf)
})
