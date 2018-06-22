context("bh_gamma_neg")

test_that("bh_gamma_neg works", {
  expect_error(bh_gamma_neg(1,-1))
  expect_equal(bh_gamma_neg(1,-0.5), 0.1781477117815606902)
  expect_equal(bh_gamma_neg(0,-0.5), Inf)
  expect_equal(bh_gamma_neg(Inf, 0.5), 0)
  expect_equal(bh_gamma_neg(1e6,-0.5), 0)
})
