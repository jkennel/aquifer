# ==============================================================================
#' @title
#' fftw_convolve
#'
#' @description
#' Designed to be used for fast moving averages 
#'
#' @param x the data series
#' @param y the filter
#' @param normalize do you want the filter normalized
#'
#' @return grf solution for multiple pumping periods
#' @export
#' @example 
#' set.seed(37) 
#' len <- 1000
#' x <- cumsum(rnorm(len))
#' y <- c(1,5,1)
#' 
#' plot(1:len, x, type='l', pch = 20, xlim= c(470, 530), ylim = c(-5, 5))
#' points(1:len, fftw_convolve(x, y), type = 'l', col='red')
fftw_convolve <- function(x, y, normalize = TRUE) {
  
  n_x <- length(x)
  n_y <- length(y)
  
  if(n_y > n_x) {
    stop('The length of y should be less than or equal to x')
  }
  
  if(normalize) {
    y <- y / sum(y)
  }
  
  x_pad <- ceiling(n_x / 2)
  y_pad <- x_pad + ceiling((n_x - n_y) / 2)
  
  
  sub <- c((n_x + x_pad):(n_x + 2 * x_pad), 1:(n_pad-1))
  
  if(n_y %% 2 == 0){
    warning('Values are shifted 0.5 units forward. Use odd number for better centering')
    u <- Re(fftw::IFFT(fftw::FFT(c(rep(0.0, y_pad), y, rep(0.0, y_pad)))  *
                         fftw::FFT(c(rep(0.0, x_pad), x, rep(0.0,x_pad)))))[sub]
    u[1:(n_y / 2)] <- NA_real_  
    u[(n_x - n_y / 2):(n_x)] <- NA_real_
    
  } else {
    u <- Re(fftw::IFFT(fftw::FFT(c(rep(0.0, y_pad-1), y, rep(0.0, y_pad)))  *
                         fftw::FFT(c(rep(0.0, x_pad), x, rep(0.0, x_pad)))))[sub]
    u[1:(n_y / 2)] <- NA_real_  
    u[(n_x - n_y / 2):(n_x)] <- NA_real_
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
#' @param plan the FFTW plan, can be missing
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
                         flow_dimension = 2,
                         plan){

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

  if(missing(plan)) {

    u <- Re(fftw::IFFT(fftw::FFT(c(rep(0.0, n_pad), coefs, rep(0.0, n_pad)))  *
                         fftw::FFT(c(rep(0.0, n_pad), u, rep(0.0,n_pad)))))[sub]
  } else {
    #print('plan')
    u <- Re(fftw::IFFT(fftw::FFT(c(rep(0.0, n_pad), coefs, rep(0.0, n_pad)), plan = plan)  *
                         fftw::FFT(c(rep(0.0, n_pad), u, rep(0.0,n_pad)), plan = plan), plan = plan))[sub]
  }

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
