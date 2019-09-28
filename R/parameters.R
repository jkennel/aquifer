#' parameters
#'
#' @param frequency the frequency in cycles per time (example units: cycles per day)
#' @param period the period of signal (example units: days)
#' @param omega the angular frequency \eqn{2 \pi \omega} (example units: radians / time)
#' @param alpha_w the dimensionless frequency
#' @param storage_aquifer the aquifer specific storage
#' @param storage_confining the confining layer specific storage
#' @param specific_yield the specific yield at the water table
#' @param transmissivity_aquifer the aquifer transmissivity
#' @param diffusivity_vadose pneumatic diffusivity of vadose zone
#' @param diffusivity_aquifer aquifer diffusivity
#' @param diffusivity_confining confining layer diffusivity
#' @param thickness_vadose the thickness of the vadose zone
#' @param thickness_aquifer the aquifer thickness
#' @param thickness_confining the confining layer thickness
#' @param height_water the depth from the water table
#' @param radius_well the radius at the screened portion of the well
#' @param radius_casing the radius at the location of the water level
#' @param loading_efficiency static loading efficiency
#' @param attenuation the attenuation factor of the capillary fringe
#' @param inverse if true water level follows the inverse of a barometric pressure change
#' @param gravity the acceleration due to gravity
#'
#' @return
#' @export
#'
parameters <- function(frequency,
                       period,
                       omega,
                       alpha_w,
                       storage_aquifer,
                       storage_confining,
                       specific_yield,
                       transmissivity_aquifer,
                       diffusivity_vadose,
                       diffusivity_aquifer,
                       diffusivity_confining,
                       thickness_vadose,
                       thickness_aquifer,
                       thickness_confining,
                       height_water,
                       radius_well,
                       radius_casing,
                       loading_efficiency,
                       attenuation,
                       inverse,
                       gravity) {

}
