context("ogata_banks")

test_that("ogata_banks works", {
  expect_equal(ogata_banks_ind(D = 2.4e-7, v = 2.6e-7, C0 = 725, x = 15, t = 3.15e7), 39.15044, tolerance = 1e-5)
  expect_equal(ogata_banks_ind(D = 2.4e-7, v = 2.6e-7, C0 = 725, x = 15, t = 3.15e7),
               ogata_banks(D = 2.4e-7, R = 1, decay = 0, v = 2.6e-7, C0 = 725, x = c(15, 20), t = c(3.15e7, 2, 4))[1,1])
})

