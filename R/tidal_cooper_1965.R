
#' tidal_cooper_1965
#'
#' @param frequency
#' @param storativity
#' @param transmissivity
#' @param thickness_aquifer
#' @param height_water
#' @param radius_well
#' @param gravity
#'
#' @return
#' @export
#' @examples
#' data('hsieh_1987_fig_2_3')
#' storativity <- 1e-07
#' transmissivity <- 1e-03
#' radius_well <- 0.05
#' frequency <- 10^seq(-5, 2, by = 0.05)
#' tau   <- 1 / frequency
#' cooper <- tidal_cooper_1965(frequency, storativity, transmissivity, thickness_aquifer = 1, height_water = 1, radius_well)
#' plot(Mod(response)~dimensionless_frequency, cooper,
#'  type='l',
#'  log = 'x',
#'  xlim = c(1, 1000))
#' points(response~dimesionless_frequency, hsieh_1987_fig_2_3[variable=='gain' & S == storativity])
#'
#' plot(unwrap(Arg(response)) * 180/pi~dimensionless_frequency, cooper,
#'  type='l',
#'  log = 'x',
#'  xlim = c(1, 1000),
#'  ylim = c(0, -90))
#' points(response~dimesionless_frequency, hsieh_1987_fig_2_3[variable=='phase' & S == storativity])
#'
tidal_cooper_1965 <- function(frequency,
                              storativity,
                              transmissivity,
                              thickness_aquifer,
                              height_water,
                              radius_well,
                              radius_casing = radius_well,
                              gravity =  9.80665) {

  h_e   <- .calc_effective_height(height_water, thickness_aquifer)
  omega <- .calc_omega(frequency)
  alpha <- .calc_alpha_w(omega, storativity, transmissivity, radius_well)
  t1    <- .calc_dimensionless_frequency(omega, radius_casing, transmissivity)


  kel   <- kelvin(alpha, nSeq = 1)

  ker_0 <- kel[['ker_0']]
  kei_0 <- kel[['kei_0']]

  # Equation 28
  e <- 1.0 - (t1 * kei_0) - ((omega)^2 * h_e) / gravity
  f <-        t1 * ker_0

  out <- data.table::data.table(frequency = frequency, period = 1/frequency)
  out[, dimensionless_frequency := transmissivity / (frequency * radius_casing^2)]
  out[, Q := .calc_dimensionless_frequency(omega, height_water, transmissivity/storativity)]
  out[, response := 1.0 / (e + f * 1i )]  # convert to imaginary
  out[, vertical_motion := response * 4 * pi^2 * h_e / (period^2 * gravity)]

}



#' #' fit_tidal_cooper_1965
#' #'
#' #' Estimating the hydraulic parameters of a confined aquifer based
#' #' on the response of groundwater levels to seismic Rayleigh waves
#' #' Xiaolong Sun, Yang Xiang, and Zheming Shi equation 9
#' #'
#' #' @param gain
#' #' @param period
#' #' @param wave_velocity
#' #' @param h_e effective water height
#' #' @param ew bulk modulus of elasticity for water
#' #' @param density_water
#' #' @param gravity
#' #' @param porosity
#' #'
#' #' @return
#' #' @export
#' fit_tidal_cooper_1965 <- function(gain,
#'                                   storativity,
#'                                   period,
#'                                   wave_velocity,
#'                                   height_water = 10,
#'                                   thickness_aquifer = 10,
#'                                   elasticity_water = 2.2e9,
#'                                   density_water = 1000,
#'                                   gravity = 9.80665,
#'                                   porosity = 0.1,
#'                                   radius_well = 0.0254,
#'                                   radius_casing = radius_well) {
#'
#'   new_periods <- seq(min(period), max(period), length.out = 50)
#'   new_gain <- approx(x = period, y = gain, xout = new_periods)
#'
#'   r       <- 2.7 * elasticity_water /
#'     (density_water * gravity * porosity * wave_velocity * period)
#'   a       <-  gain / r
#'   a_prime <- a * 4 * pi^2 * height_water / (period^2 * gravity)
#'
#'   to_minimize <- function(par, a, a_prime, storativity, period, thickness_aquifer,
#'                           height_water,radius_well, radius_casing, gravity) {
#'
#'     resp <- tidal_cooper_1965(frequency = 1/period,
#'                              storativity, # storativity
#'                              10^par[2], # transmissivity
#'                              thickness_aquifer,
#'                              height_water,
#'                              radius_well,
#'                              radius_casing = radius_casing,
#'                              gravity = gravity)
#'
#'     # Calculate the complex difference
#'
#'     a_mod <- Mod(resp$response)
#'     a_prime_mod <- Mod(resp$vertical_motion)
#'
#'     sum((a_mod - a)^2 + (a_prime_mod - a_prime)^2)
#'
#'
#'   }
#'
#'   par <- c(-4)
#'   res <- optim(fn = to_minimize,
#'                par = par,
#'                lower = c(-6),
#'                upper = c(1),
#'                method = 'L-BFGS-B',
#'                gain = gain,
#'                phase = phase,
#'                frequency = frequency,
#'                radius_well = radius_well,
#'                radius_casing = radius_casing)
#'
#'
#'
#'   return(data.table(period, a, a_prime, r))
#'
#' }

#
# frequency = 10^seq(-10,1,0.01)
# tmp <- tidal_cooper_1965(frequency,
#                   storativity = 2.4e-6,
#                   transmissivity = 0.00375*8,
#                   thickness_aquifer = 8,
#                   height_water = 15,
#                   radius_well = 0.054,
#                   radius_casing = 0.054,
#                   gravity =  9.80665)
#
# plot(Mod(response)~period, tmp, type='l', log = 'y',
#      xlim = c(0, 50),
#      ylim = c(1e-3, 10))
# plot(Mod(vertical_motion)~period, tmp, type='l', log = 'y',
#      xlim = c(0, 50),
#      ylim = c(1e-3, 10))
