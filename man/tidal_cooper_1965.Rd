% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidal_cooper_1965.R
\name{tidal_cooper_1965}
\alias{tidal_cooper_1965}
\title{tidal_cooper_1965}
\usage{
tidal_cooper_1965(frequency, storativity, transmissivity,
  thickness_aquifer, height_water, radius_well,
  radius_casing = radius_well, gravity = 9.80665)
}
\arguments{
\item{gravity}{}
}
\description{
tidal_cooper_1965
}
\examples{
data('hsieh_1987_fig_2_3')
storativity <- 1e-07
transmissivity <- 1e-03
radius_well <- 0.05
frequency <- 10^seq(-5, 2, by = 0.05)
tau   <- 1 / frequency
cooper <- tidal_cooper_1965(frequency, storativity, transmissivity, thickness_aquifer = 1, height_water = 1, radius_well)
plot(Mod(response)~dimensionless_frequency, cooper,
 type='l',
 log = 'x',
 xlim = c(1, 1000))
points(response~dimesionless_frequency, hsieh_1987_fig_2_3[variable=='gain' & S == storativity])

plot(unwrap(Arg(response)) * 180/pi~dimensionless_frequency, cooper,
 type='l',
 log = 'x',
 xlim = c(1, 1000),
 ylim = c(0, -90))
points(response~dimesionless_frequency, hsieh_1987_fig_2_3[variable=='phase' & S == storativity])

}
