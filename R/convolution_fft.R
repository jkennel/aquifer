# ==============================================================================
#' @title
#' fftw_convolve
#'
#' @description
#' Designed to be used for fast centered moving averages
#'
#' @param x the data series
#' @param y the filter
#' @param normalize do you want the filter normalized
#' @param align what alignment
#'
#' @return convolution of data series and filter
#'
#' @export
#'
#' @examples
#' set.seed(37)
#' len <- 999
#' x <- cumsum(rnorm(len))
#' y <- rep(1, 3)
#' sub <- c(1, 50)
#' plot(1:len, x, type='o', pch = 20, xlim = sub, cex = 0.4)
#' points(1:len, fftw_convolve(x, y), type = 'o', col='red', pch = 20, cex = 0.4)
fftw_convolve <- function(x, y, normalize = TRUE, align = 'center') {

  n_x_in <- length(x)
  n_y    <- length(y)

  y <- rev(y)

  if(n_y > n_x_in) {
    stop('The length of y should be less than or equal to x')
  }

  # set NA indices
  start <- 1:(ceiling(n_y / 2) - 1)
  end   <- (n_x_in - floor(n_y / 2) + 1):(n_x_in)


  if(n_x_in %% 2 == 1) {
    x     <- c(x, 0.0)
    n_x   <- length(x)
    x_pad <- n_x / 2
    sub   <- c((n_x + x_pad + 1):(n_x + 2 * x_pad), 1:(x_pad-1))
  } else {
    n_x   <- n_x_in
    x_pad <- n_x / 2
    sub   <- c((n_x + x_pad + 1):(n_x + 2 * x_pad), 1:(x_pad))
  }

  # normalize filter to sum to 1
  if(normalize) {
    y <- y / sum(y)
  }

  y_pad <- x_pad + ceiling((n_x - n_y) / 2)

  x_pad <- rep(0.0, x_pad)
  y_pad <- rep(0.0, y_pad)

  if(n_y %% 2 == 0){

    warning('Values are shifted 0.5 units forward. Use odd number filter for better centering')
    u <- Re(fftw::IFFT(fftw::FFT(c(y_pad, y, y_pad))  *
                         fftw::FFT(c(x_pad, x, x_pad))))[sub]

  } else {
    u <- Re(fftw::IFFT(fftw::FFT(c(y_pad, y, y_pad[-1]))  *
                       fftw::FFT(c(x_pad, x, x_pad))))[sub]

  }


  u[start] <- NA_real_
  u[end]   <- NA_real_

  if (align == 'right') {
    u <- data.table::shift(u, n = length(end), type = 'lag')
  } else if (align == 'left') {
    u <- data.table::shift(u, n = length(start), type = 'lead')
  }

  return(u)
}


# ==============================================================================
#' @title
#' grf_convolve
#'
#' @description
#' Implementation of Barker, 1988 Generalized Radial Flow model for line source/sink.  For each time given a flow_rate needs to be provided.
#'
#' @param radius distance to monitoring location
#' @param storativity the storativity of the aquifer
#' @param K the hydraulic conductivity of the aquifer
#' @param thickness the thickness
#' @param time the time to output
#' @param flow_rate vector of flow rates
#' @param flow_dimension the barker flow dimension
#'
#' @return grf solution for multiple pumping periods
#' @export
#'
grf_convolve <- function(radius,
                         storativity,
                         K,
                         thickness,
                         time,
                         flow_rate,
                         flow_dimension = 2) {

  v = (flow_dimension / 2) - 1

  #coefs <- well_function_coefficient(flow_rate,  transmissivity)
  coefs <- grf_coefficient(flow_rate, radius, K, thickness, flow_dimension)

  # calculate the Well function
  u <- grf_u_time(radius, storativity, K, time)
  u <- grf_parallel(u, v)

  # calculate the pulse
  u <- impulse_function(u, 1)
  n_pad <- ceiling(length(u) / 2)
  sub <- (2 * n_pad + 1):(2 * n_pad + length(u))

  u <- Re(fftw::IFFT(fftw::FFT(c(rep(0.0, n_pad), coefs, rep(0.0, n_pad)))  *
                     fftw::FFT(c(rep(0.0, n_pad), u, rep(0.0,n_pad)))))[sub]
}


# ==============================================================================
#' @title
#' hantush_convolve
#'
#' @description
#' Implementation of Hantush leaky aquifer solution
#'
#' @param radius distance to monitoring location
#' @param storativity the storativity of the aquifer
#' @param transmissivity the transmissivity of the aquifer
#' @param leakage the leakage rate of the aquitard
#' @param time the time to output
#' @param flow_rate vector of flow rates
#' @param n_terms number of terms in the hantush well function approximation
#'
#' @return hantush solution for multiple pumping periods
#' @export
#'
hantush_convolve <- function(radius,
                             storativity,
                             transmissivity,
                             leakage,
                             time,
                             flow_rate,
                             n_terms=10){


  coefs <- well_function_coefficient(flow_rate,  transmissivity)
  b <- hantush_epsilon(radius, leakage)

  # calculate the Well function
  u <- theis_u_time(radius, storativity, transmissivity, time)
  u <- hantush_well_parallel(u, b, n_terms)

  # calculate the pulse
  u <- impulse_function(u, 1)

  # do the convolution
  n_pad <- ceiling(length(u)/2)
  sub <- (2 * n_pad + 1):(2 * n_pad + length(u))

  u <- Re(fftw::IFFT(fftw::FFT(c(rep(0.0,n_pad),coefs,rep(0.0,n_pad)))  *
                     fftw::FFT(c(rep(0.0,n_pad),u,rep(0.0,n_pad)))))[sub]
}
