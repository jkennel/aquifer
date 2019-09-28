context("fftw_convolve")

test_that("fftw_convolve works", {
  vals <- c(1,2,1,0)
  expect_equal(fftw_convolve(vals,c(0.1, 0.2, 0.1)), c(NA, 1.5, 1.0, NA))
  vals <- c(1,2,1)
  filt <- c(0.1, 0.2, 0.1)
  expect_equal(fftw_convolve(vals, filt), c(NA, 1.5, NA))
  filt <- c(0.1, 0.2, 0.1, 0.1)
  expect_error(fftw_convolve(vals, filt))
  vals <- c(1,2,2,1,1)
  filt <- c(0, 0, 1, 0)
  expect_warning(fftw_convolve(vals, filt))

  x <- rnorm(24)
  y <- abs(rnorm(6))

  expect_equal(RcppRoll::roll_mean(x, n = 6, weights = y, fill = 'NA', align = 'center', normalize = TRUE),
               fftw_convolve(x, y, TRUE))

  x <- rnorm(23)
  y <- abs(rnorm(6))

  expect_equal(RcppRoll::roll_mean(x, n = 6, weights = y, fill = 'NA', align = 'center', normalize = TRUE),
               fftw_convolve(x, y, TRUE))

  x <- rnorm(24)
  y <- abs(rnorm(7))
  expect_equal(RcppRoll::roll_mean(x, n = 7, weights = y, fill = 'NA', align = 'center', normalize = TRUE),
               fftw_convolve(x, y, TRUE))

  x <- rnorm(23)
  y <- abs(rnorm(7))
  expect_equal(RcppRoll::roll_mean(x, n = 7, weights = y, fill = 'NA', align = 'center', normalize = TRUE),
               fftw_convolve(x, y, TRUE))




  x <- rnorm(23)
  y <- abs(rnorm(7))
  expect_equal(RcppRoll::roll_mean(x, n = 7, weights = y, fill = 'NA', align = 'left', normalize = TRUE),
               fftw_convolve(x, y, TRUE, align = 'left'))

  x <- rnorm(23)
  y <- abs(rnorm(7))
  expect_equal(RcppRoll::roll_mean(x, n = 7, weights = y, fill = 'NA', align = 'right', normalize = TRUE),
               fftw_convolve(x, y, TRUE, align = 'right'))



})
