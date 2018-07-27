#' para
#'
#' This function returns the reciprocal of the sum of the reciprocals of all the
#' values present in its arguments.
#' @param ... numeric or complex logical vectors passed as separate arguments
#' @keywords
#' @export
#' @examples
#' para(1, 2, 3, 4)
#' para(c(1 + 2i, 1 + 3i), c(2 + 1i, 1 - 3i), c(4 + 2i, 1 + 2i))
#' 

para <- function(...) {
  args <- list(...)

  Reduce(`+`, lapply(args, function(n) 1/n))^-1 
}

#' omega_range
#' 
#' This function calculates a range of values for angular frequency (omega) from
#' specified limits of log frequency with a given number of points per decade of
#' frequency.
#' @param low.logf The log of the minimum frequency in Hz
#' @param high.logf The log of the maximum frequency in Hz
#' @param p.per.dec The number of points per decade.
#' @keywords
#' @export
#' @examples 
#' omega.range(-1, 6, 10)

omega_range <- function(low.logf, high.logf, p.per.dec) {
  2 * pi * 10^seq(low.logf, high.logf, length.out = (p.per.dec * (high.logf - low.logf)) + 1)
}

#' Z_to_df
#' 
#' Takes a complex vector and returns a data frame of the real and imaginary parts
#' as separate columns.
#' @param Z A complex vector.
#' @keywords
#' @export
#' @examples 
#' Z_to_df(para(Z_R(10), Z_C(1E-4, omega.range(-1, 6, 10))))

Z_to_df <- function(Z) {
  return(data.frame(Re = Re(Z), Im = Im(Z)))
}

