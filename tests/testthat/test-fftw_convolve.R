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
})
