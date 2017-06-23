

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
