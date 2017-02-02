context("gamma_der")

test_that("gamma_der works", {

  expect_equal(gamma_der(2, 0),
               (exp_int_single(1.99999, 0) - exp_int_single(2.00001, 0)) / 0.00002)
  expect_equal(gamma_der(2, -0.5),
               (bh_gamma_neg(1.99999, -0.5) - bh_gamma_neg(2.00001, -0.5)) / 0.00002)
  expect_equal(gamma_der(2, 0.5),
               (bh_tgamma(1.99999, 0.5) - bh_gamma_neg(2.00001, 0.5)) / 0.00002)

  expect_equal(gamma_der(0, 0), Inf)
  expect_equal(gamma_der(0, -0.5), Inf)
  expect_equal(gamma_der(0, 0.5), Inf)

  expect_equal(gamma_der(Inf, 0), 0)
  expect_equal(gamma_der(Inf, -0.5), 0)
  expect_equal(gamma_der(Inf, 0.5), 0)

})
