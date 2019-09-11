test_that("den_iseger_data works", {
  
  expect_equal(as.numeric(iseger_data(16)[1,]), 
               c(4.44089209850063e-016,1.0))
  
  expect_equal(as.numeric(iseger_data(16)[8,]), 
               c(170.533131190126, 54.9537264520382))
  
  
  expect_equal(as.numeric(iseger_data(32)[1,]), c(0.0,1.0))
  
  expect_equal(as.numeric(iseger_data(32)[16,]), 
               c(669.650134867713, 213.824023377988))
  
  
  
  expect_equal(as.numeric(iseger_data(48)[1,]), c(0.0,1.0))
  
  expect_equal(as.numeric(iseger_data(48)[24,]), 
               c(1494.71066227687, 476.448331869636))
  
  
  expect_error(iseger_data(8), regexp = 'n must be 16, 32, or 48')
})




test_that("den_iseger works", {

  
  s <- seq(0, 2, 0.1) 
  
  
  Lf <- function(s) 1/s
  f  <- function(t) rep(1, length(t))
  expect_equal(f(s)[-1], den_iseger(Lf, s, 16)[-1])
  
  
  Lf <- function(s) s^(-2)
  f  <- function(t) t
  expect_equal(f(s)[-1], den_iseger(Lf, s, 16)[-1])
  
  
  Lf <- function(s) 1/(s^2 + 1)
  f  <- function(t) sin(t)
  expect_equal(f(s)[-1], den_iseger(Lf, s, 16)[-1])
  
  
  Lf <- function(s) (s^2-1)*(s^2+1)^(-2)
  f  <- function(t) t*cos(t)
  expect_equal(f(s)[-1], den_iseger(Lf, s, 16)[-1])
  
  
  Lf <- function(s) (s^2+1)^(-0.5)
  f  <- function(t) besselJ(t, 0)
  expect_equal(f(s)[-1], den_iseger(Lf, s, 16)[-1])
  
  
  Lf <- function(s) (s + 0.5)^(-1)
  f  <- function(t) exp(-t/2)
  expect_equal(f(s)[-1], den_iseger(Lf, s, 16)[-1])
  
  
  Lf <- function(s) s^(-0.5)
  f  <- function(t) (pi * t)^(-0.5) 
  expect_equal(f(s)[-1], den_iseger(Lf, s, 16)[-1], tolerance = 5e-4)
  
  
  Lf <- function(s) s^(-0.5) * exp(-s^(-1))
  f  <- function(t) (pi * t)^(-0.5) * cos(2 * sqrt(t))
  expect_equal(f(s)[-1], den_iseger(Lf, s, 16)[-1], tolerance = 1e-4)
  

  Lf <- function(s) atan(1 / s)
  f  <- function(t) sin(t) / t
  expect_equal(f(s)[-1], den_iseger(Lf, s, 16)[-1])
  
  
  Lf <- function(s) exp(-4 * sqrt(s))
  f  <- function(t) 2 * exp(-4/t)*((pi*t^3)^(-0.5))
  expect_equal(f(s)[-1], den_iseger(Lf, s, 16)[-1])
  
  
  # library(fBasics)
  # Lf <- function(s) s^(-1) * exp(-s)
  # f  <- function(t) ifelse(t >= 1, 1.0, 0.0)
  # expect_equal(f(s)[-1], den_iseger(Lf, s, 16)[-1])

  
  
  
})

