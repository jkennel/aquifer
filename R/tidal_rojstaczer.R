# Intermediate Period Response of Water Wells to Crustal Strain
# Rojstaczer, 1988
# A3a, A3b, A4a, A4b

tidal_rojstaczer_1988 <- function(frequency,
                                  storativity,
                                  transmissivity,
                                  height_water) {

  omega <- .calc_omega(frequency)

  # Equation 11
  Q_prime <- .calc_Q(omega,
                     height_water,
                     transmissivity/storativity)

  sqrt_Q_prime <- sqrt(Q_prime)

  # Intermediate Period Response of Water Wells to Crustal Strain
  # Rojstaczer, 1988
  # A3a, A3b, A4a, A4b

  t1 <- exp(-sqrt_Q_prime)

  e <- t1 * cos(sqrt_Q_prime) - 1
  f <- t1 * sin(sqrt_Q_prime)

  data.table(frequency,
             Q_prime,
             response = e + f * -1i)

}


# Rojstaczer Figure 3
# frequency <- 10^seq(-8, 6, 0.1)
# storativity <- 1e-5
# transmissivity <- 1e-4
# height_water <- 50
# tmp <- tidal_rojstaczer_1988(frequency,
#                  storativity,
#                  transmissivity,
#                  height_water)
# library(data.table)
# gain <- fread('/media/kennel/Data/tmp/rojstaczer_tidal_gain.csv')
# setnames(gain, c('Q_prime', 'gain'))
# plot(gain~Q_prime, gain, log = 'x')
# points(Mod(response)*0.05~Q_prime, tmp, type='l')
#
#
#
#
# phase <- fread('/media/kennel/Data/tmp/rojstaczer_tidal_phase.csv')
# setnames(phase, c('Q_prime', 'phase'))
# plot(phase~Q_prime, phase, log = 'x')
# points(unwrap(Arg(response))*180/pi~Q_prime, tmp, type='l', log = 'x')

