context("bh_tgamma")

test_that("bh_tgamma works", {
  expect_error(bh_tgamma(2.5,0))
  expect_equal(bh_tgamma(2.5,1), 0.082084998623898)
  expect_equal(bh_tgamma(0,1), 1.0)
  expect_equal(bh_tgamma(1e6,1), 0)
  expect_equal(bh_tgamma(10,2), 4.99399273873333668915e-4)
})
