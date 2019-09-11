#' kelvin
#' Kelvin functions of the second kind ker and kei and order 0 to 1.
#'
#' @param z value to evaluate the kelvin functions
#'
#' @return data.table of real and imaginary kelvin functions
#'
#' @importFrom data.table ":=" "data.table" "setnames"
#'
#' @export
kelvin <- function(z, nSeq = 2) {

  n  <- 0:(nSeq-1)
  c1 <- exp(1i * pi / 4.0)
  c2 <- matrix(zapsmall(exp(-pi * n * 1i / 2.0)),
               ncol = nSeq,
               nrow = length(z),
               byrow = TRUE)

  k  <- data.table::as.data.table(c2 * Bessel::BesselK(z * c1, nu = 0, nSeq = nSeq))

  nms_k   <- paste0('k_', n)
  nms_ker <- paste0('ker_', n)
  nms_kei <- paste0('kei_', n)
  data.table::setnames(k, nms_k)

  k[, (nms_ker) := lapply(mget(nms_k), Re)]
  k[, (nms_kei) := lapply(mget(nms_k), Im)]

  return(k)

}
