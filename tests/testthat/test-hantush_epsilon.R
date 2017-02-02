context("hantush epsilon")

test_that("hantush_epsilon works", {
  expect_equal(hantush_epsilon(1e-4, 1e6), ((1^2) / (4 * (1e6)^2)))
  expect_equal(hantush_epsilon(1e6, 1e-4), (1e6)^2 / (4 * (1e-4)^2))
  expect_equal(hantush_epsilon(0, 1e-4), 0)
  expect_equal(hantush_epsilon(1e-4,0), Inf)

})
