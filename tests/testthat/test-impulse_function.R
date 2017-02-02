context("impulse_function")


test_that("impulse_function works", {

  expect_equal(impulse_function(1:100, 10), c(c(1:10), rep(10,90)))
  expect_equal(impulse_function(1:50, 1), rep(1, 50))
  expect_equal(impulse_function(0, 1), 0)

})
