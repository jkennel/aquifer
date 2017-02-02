context("ogata_banks")

test_that("ogata_banks works", {
  expect_equal(ogata_banks(D = 2.4e-7, v = 2.6e-7, C0 = 725, x = 15, t = 3.15e7), 39.15044, tolerance = 1e-5)
})

