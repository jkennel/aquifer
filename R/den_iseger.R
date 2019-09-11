# Numerical Inversion of Laplace Transform for Time Resolved Thermal Characterization Experiment.
# It appears that it needs to start at zero and need regularly spaced times.
#' den_iseger
#'
#' @param impulse
#' @param time must be regularly spaced
#' @param n_weights
#'
#' @return
#' @export
#'
#' @examples
den_iseger <- function(impulse, time, n_weights = 16, ...) {


  n_time <- length(time)
  dt <- max(time) / (n_time - 1)

  # get weights
  i_data <- iseger_data(n_weights)
  li = i_data$li
  bi = i_data$bi

  n <- 8 * n_time
  a <- 44 / n

  # pg 33
  c <- 2 * 1i * pi / n
  li <- a + 1i * li
  k <- 0:n

  s2 <- rep(0.0, length(k))

  for (j in 1:length(li)) {

    s <- (li[j] + c * k) / dt

    ft <- Re(impulse(s, ...))
    s2 <- s2 + bi[j] * ft

  }

  s2 <- s2 * 4 / dt

  s2[1] <- 0.5 * (s2[1] + s2[n + 1])
  s2 <- fft(s2[1:n], inverse = TRUE) / (n)

  Re(exp(a * (0:(n_time - 1))) * (s2[1:n_time]))

}



#' iseger_data
#'
#'
#' @param n number of terms divided by 2
#'
#' @return iseger weights
#' @export
#'
#' @examples
#' iseger_data(16)
iseger_data <- function(n = 16) {

  if (n == 16) {
    iseger = data.frame(
      li = c(
        0.00000000000000,
        6.28318530717958,
        12.5663706962589,
        18.8502914166954,
        25.2872172156717,
        34.2969716635260,
        56.1725527716607,
        170.533131190126
      ),
      bi = c(
        1.00000000000000,
        1.00000000000004,
        1.00000015116847,
        1.00081841700481,
        1.09580332705189,
        2.00687652338724,
        5.94277512934943,
        54.9537264520382
      )
    )
  } else if (n==32) {


    iseger <- data.frame(
      li = c(
        0.00000000000000,
        6.28318530717958,
        12.5663706143592,
        18.8495559215388,
        25.1327412287184,
        31.4159265359035,
        37.6991118820067,
        43.9823334683971,
        50.2716029125234,
        56.7584358919044,
        64.7269529917882,
        76.7783110023797,
        96.7780294888711,
        133.997553190014,
        222.527562038705,
        669.650134867713
      ),
      bi = c(
        1.00000000000000,
        1.00000000000000,
        1.00000000000000,
        1.00000000000000,
        1.00000000000000,
        1.00000000000895,
        1.00000004815464,
        1.00003440685547,
        1.00420404867308,
        1.09319461846681,
        1.51528642466058,
        2.41320766467140,
        4.16688127092229,
        8.37770013129610,
        23.6054680083019,
        213.824023377988
      )
    )
  } else if (n == 48) {

    iseger <- data.frame(
      li = c(
        0.00000000000000,
        6.28318530717957,
        12.5663706143592,
        18.8495559215388,
        25.1327412287183,
        31.4159265358979,
        37.6991118430775,
        43.9822971502571,
        50.2654824574367,
        56.5486677646182,
        62.8318530747628,
        69.1150398188909,
        75.3984537709689,
        81.6938697567735,
        88.1889420301504,
        95.7546784637379,
        105.767553649199,
        119.587519367740,
        139.158762677521,
        168.156165377339,
        214.521886792255,
        298.972429369901,
        497.542914576338,
        1494.71066227687
      ),
      bi = c(
        1.00000000000000,
        1.00000000000000,
        1.00000000000000,
        1.00000000000000,
        1.00000000000000,
        1.00000000000000,
        1.00000000000000,
        1.00000000000000,
        1.00000000000000,
        1.00000000000234,
        1.00000000319553,
        1.00000128757818,
        1.00016604436873,
        1.00682731991922,
        1.08409730759702,
        1.36319173228680,
        1.85773538601497,
        2.59022367414073,
        3.73141804564276,
        5.69232680539143,
        9.54600616545647,
        18.8912132110256,
        52.7884611477405,
        476.448331869636
      )
    )
  } else {
    stop('n must be 16, 32, or 48')
  }

  iseger_end <- iseger
  iseger_end$bi <- rev(iseger_end$bi)
  iseger_end$li <- -rev(iseger_end$li) - (2 * pi)

  #return(rbind(iseger, iseger_end))
  return(iseger)
}




# time <- 5.776 * seq(0, 1, 0.001)
# aa <- 5.776 * seq(0, 10, 1)/7.6^2
# system.time(
#
# tmp1 <- den_iseger(time = time,
#            n_weights = 48,
#            impulse = .slug_cbp_laplace_complex,
#            radius_casing = 7.6,
#            radius_screen = 7.6,
#            radius = 7.6,
#            storativity = 1e-5,
#            transmissivity = 1,
#            head_0 = 1)
# )
#
# system.time(
#
# tmp2 <- stehfest(time = time+1e-03,
#                   n = 12,
#                   impulse = .slug_cbp_laplace,
#                   radius_casing = 7.6,
#                   radius_screen = 7.6,
#                   radius = 7.6,
#                   storativity = 1e-5,
#                   transmissivity = 1,
#                   head_0 = 1)
# )
#
# tmp1[2]
# tmp2[1]
#
# plot(tmp1~time, type='l', col = 'red', log = 'x')
# points(tmp2~time, type='l')



# library(microbenchmark)
#
# microbenchmark(
#   Bessel::BesselK(complex(real = 1, imaginary = 0), 1),
#   bessel_k_single(1, 1)
# )







