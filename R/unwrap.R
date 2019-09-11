#' unwrap
#' Removes the large phase shifts that can be associated with using Arg or atan2.
#'
#' @param phase the phase in radians
#'
#' @return
#' @export
#'
unwrap <- function(phase) {

  d <- c(0, -diff(phase))
  s <- 2 * pi * (((d > pi) > 0) - ((d < -pi) > 0))

  phase + cumsum(s)

}


