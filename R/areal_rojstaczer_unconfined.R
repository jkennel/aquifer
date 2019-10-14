
#' @inheritParams parameters
#' @export
areal_rojstaczer_unconfined <- function(frequency,
                                        radius_well,
                                        transmissivity,
                                        storage_aquifer,
                                        specific_yield,
                                        k_vertical,
                                        diffusivity_vertical,
                                        diffusivity_vadose,
                                        thickness_saturated_well,
                                        thickness_vadose,
                                        thickness_aquifer,
                                        loading_efficiency,
                                        attenuation) {


  omega <- .calc_omega(frequency)
  R     <- .calc_dimensionless_frequency(omega, thickness_vadose, diffusivity_vadose)
  Qu    <- .calc_dimensionless_frequency(omega, thickness_saturated_well, diffusivity_vertical)

  sqrt_R  <- sqrt(R)
  sqrt_Qu <- sqrt(Qu)

  mn      <- .calc_mn(sqrt_R, attenuation)
  ohm     <- .calc_ohm(omega, storage_aquifer, k_vertical, specific_yield)
  h1_h2   <- .calc_H1_H2(omega, 0, thickness_aquifer, diffusivity_vertical)
  uv      <- .calc_u_v(sqrt_Qu, h1_h2[['h1']], h1_h2[['h2']])

  print(uv)
  p0 <- ((mn$m - 1i * mn$n) - loading_efficiency) *
    (uv$u + 1i * uv$v) + loading_efficiency


  x0 <- (-1 + p0)


  data.table(frequency,
             Qu,
             sqrt_Qu,
             R,
             sqrt_R,
             response = x0)

}
