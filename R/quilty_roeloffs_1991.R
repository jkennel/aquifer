# library(data.table)
# tf <- fread('/media/kennel/Data/tmp/turkey_flat.csv')
# setnames(tf, c('frequency', 'be'))
#
#
# frequency <- seq(0, 1.0, 0.01) / 86400
# omega <- frequency  * 2 * pi
# zw   <- 19
# z    <- 165
# c    <- 240 / (100*100) -> diffusivity_vertical
# ca   <- 7.5 / (100*100) -> diffusivity_vadose
# alpha <- 0.5 # (beta * (1 + vu)) / (3 *(1 - vu))
#
# sqrt_t1 <- sqrt((1i * omega) / ca)
# pa <- 2.0 / (exp( sqrt_t1 * z) +
#              exp(-sqrt_t1 * z))
#
# be <- -1 + alpha + (pa - alpha) *
#   exp(-sqrt((1i * omega) / c) * (z - zw))
#
# frequency <- (frequency * 86400)
# plot(be~frequency, tf)
# points(Mod(be)~frequency,  type='l')
#
#
#
# library(data.table)
# tf <- fread('/media/kennel/Data/tmp/stockdale_mountain.csv')
# setnames(tf, c('frequency', 'be'))
#
#
# frequency <- seq(0, 1.0, 0.01) / 86400
# omega <- frequency * (2 * pi)
# zw   <- 140 * 100
# z    <- 293 * 100
# c    <- 1100 -> diffusivity_vertical
# alpha <- 1 # (beta * (1 + vu)) / (3 *(1 - vu))
#
#
# be <- -1 + (alpha * (1 - exp(-sqrt((1i * omega) / c) * (z - zw))))
#
# frequency <- (frequency * 86400)
# plot(be~frequency, tf, ylim = c(0, 1))
# points(Mod(be)~frequency,  type='l')
#
# phase <- atan2(Im(be), Re(be)) * 180/pi-360
# plot(phase~frequency, type='l')
#
#
#
