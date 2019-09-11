# .calc_omega -------------------------------------------------------------
.calc_omega <- function(frequency) {
  2 * pi * frequency
}

# .calc_W -----------------------------------------------------------------
.calc_W <- function(omega, radius, transmissivity) {
  (radius^2 * omega) / transmissivity
}

# .calc_alpha_w -----------------------------------------------------------
.calc_alpha_w <- function(omega, storativity, transmissivity, radius_well) {
  sqrt((omega * storativity) / transmissivity) * radius_well
}


# .calc_R -----------------------------------------------------------------
.calc_R <- function(omega, thickness_vadose, diffusivity_vadose) {
  (thickness_vadose^2 * omega) / (2.0 * diffusivity_vadose)
}

# .calc_h_w ---------------------------------------------------------------
.calc_h_w <- function(height_water, thickness_aquifer) {
  height_water + (3 * thickness_aquifer) / 8
}


# .calc_Q -----------------------------------------------------------------
# dimensionless frequencies, Q, Qu
.calc_Q <- function(omega, thickness, diffusivity) {
  (thickness^2 * omega) / (2.0 * diffusivity)
}

# # .calc_Q_prime -----------------------------------------------------------
# .calc_Q_prime <- function(omega, storativity, transmissivity, height_water) {
#   (height_water^2 * omega * storativity)/ transmissivity
# }

# # .calc_Qu ----------------------------------------------------------------
# .calc_Qu <- function(omega, thickness_saturated_well, diffusivity_vertical) {
#   (thickness_saturated_well^2 * omega) / (2.0 * diffusivity_vertical)
# }


# .calc_ohm ---------------------------------------------------------------
.calc_ohm <- function(omega, storage_aquifer, k_vertical, specific_yield){
  (1.0 - 1i) * sqrt((storage_aquifer * k_vertical) /
                    (2.0 * specific_yield^2 * omega))
}

# .calc_H1 ----------------------------------------------------------------
.calc_H1_H2 <- function(omega, ohm, thickness_aquifer, diffusivity_vertical) {

  t1 <- 2.0 * (1i + 1) * (omega * thickness_aquifer^2 / (2.0*diffusivity_vertical))^0.5
  t2 <- 2.0 * (1i + 1) * (omega * thickness_aquifer^2 / (2.0 * diffusivity_vertical))^0.5

  h1 <- 1 + exp(-t1) #1.0 + exp(-t1) #- ohm * (1 - exp(-t2))
  h2 <- 1 + exp( t2) #+ ohm * (1 - exp( t2))

  list(h1 = h1, h2 = h2)

}


# .calc_u_v ---------------------------------------------------------------
.calc_u_v <- function(sqrt_Qu, h1_h2) {

  cos_Qu <- cos(sqrt_Qu)
  sin_Qu <- sin(sqrt_Qu)

  h1 <- h1_h2$h1
  h2 <- h1_h2$h2

  u_bar <- exp(-sqrt_Qu * (-cos_Qu + sin(sqrt_Qu))/ (2.0 * sqrt_Qu * h1)) +
    exp( sqrt_Qu * ( cos_Qu + sin(sqrt_Qu))/ (2.0 * sqrt_Qu * h1)) +
    ((1/h1) - (1/h2)) / (2.0 * sqrt_Qu)

  v_bar <- exp(-sqrt_Qu * (cos_Qu + sin(sqrt_Qu))/ (2.0 * sqrt_Qu * h1)) +
    exp( sqrt_Qu * (-cos_Qu + sin(sqrt_Qu))/ (2.0 * sqrt_Qu * h1)) +
    ((1/h2) - (1/h1)) / (2.0 * sqrt_Qu)

  list(u = u_bar, v = v_bar)
}



# .calc_mn ----------------------------------------------------------------
.calc_mn <- function(sqrt_r, attenuation) {

  denom  <- (cosh(2.0 * (sqrt_r)) + cos(2.0 * (sqrt_r))) / (2.0 * attenuation)
  m      <- (cosh(sqrt_r) * cos(sqrt_r)) / denom
  n      <- (sinh(sqrt_r) * sin(sqrt_r)) / denom

  list(m = m, n = n)

}

