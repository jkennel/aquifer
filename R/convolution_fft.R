# ==============================================================================
#' pad
#'
#' @param x vector of values for fft
#'
#' @return padded vector for fft
#' @export
#'
pad <- function(x){
  c(rep(0, length(x)), x, rep(0, length(x)))
}



# ==============================================================================
#' theis_convolve
#'
#' @param radius distance to monitoring location
#' @param storativity the storativity of the aquifer
#' @param transmissivity the transmissivity of the aquifer
#' @param time the time to output
#' @param flow_rate vector of flow rates
#' @param flow_dimension the barker flow dimension
#'
#' @return grf solution for multiple pumping periods
#' @export
#'
theis_convolve <- function(radius,
                           storativity,
                           transmissivity,
                           time,
                           flow_rate,
                           flow_dimension=2){

  v = 1 - flow_dimension/2

  coefs <- well_function_coefficient(flow_rate,  transmissivity)

  # calculate the Well function
  u <- theis_u_time(radius, storativity, transmissivity, time)
  u <- grf_parallel(u, v)

  # calculate the pulse
  u <- impulse_function(u, 1)

  u <- Re(fftw::IFFT(fftw::FFT(pad(coefs))  * fftw::FFT(pad(u))))[(2*length(u) + 1):(3*length(u))]

}


# ==============================================================================
#' hantush_convolve
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
  u <- Re(fftw::IFFT(fftw::FFT(pad(coefs))  * fftw::FFT(pad(u))))[(2*length(u) + 1):(3*length(u))]

}
