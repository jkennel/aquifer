#' stehfest
#'
#' @description Inverse Laplace transform using Stehfest algorithm
#'
#' @param time elapsed time since slug introduction
#' @param n coefficient for Stehfest algorithm (should be < 20)
#' @param impulse function to invert
#' @param ... arguments to pass to impulse
#'
#' @return inverse laplace transform of impulse function
#' @export
#'
#' @examples
stehfest <- function(time, n=12, impulse, ...) {

  n_div_2 <- n / 2
  ind     <- 1:n
  fac     <- factorial(0:n)
  v       <- rep(NA_real_, n)

  for (i in ind) {

    k <- min(i, n_div_2):trunc((i + 1) / 2)

    z <- sum(((k^n_div_2) * fac[2 * k + 1]) /
              (fac[n_div_2 - k + 1] * fac[k + 1] * fac[k] *
               fac[i - k + 1] * fac[2 * k - i + 1]))

    v[i] <- (-1)^((n / 2) + i) * z;

  }

  time <- log(2.0) / time
  p    <- tcrossprod(as.numeric(ind), time)
  out  <- as.vector(crossprod(impulse(p, ...), v) * time)

  return(out)
}


