#' Z_R
#' 
#' Calculates the impedance of a resistor and returns a complex number (simply a real number with
#' a resistance of the argument R).
#' @param R resistance with units of Ohms.
#' @keywords
#' @export
#' @examples
#' Z_R(10)

Z_R <- function(R) {
  complex(real = R, imaginary = 0)
}


#' Z_C
#' 
#' Calculates the impedance of a capacitor from a capacitance and a vector of values of angular
#' frequency and returns a complex vector.
#' @param C capacitance with units of Farads.
#' @param omega a vector of angular frequency values, which can be created with omega_range()
#' @keywords
#' @export
#' @examples
#' Z_C(1E-4, omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10))

Z_C <- function(C, omega) {
  Z_C <- complex(real = 0, imaginary = -1/(omega * C))
  return(Z_C)
}


#' Z_L
#' 
#' Calculates the impedance of an inductor from an inductance and a vector of values of angular
#' frequency and returns a complex vector.
#' 
#' @param L inductance with units of Henry.
#' @param omega a vector of angular frequency values, which can be created with omega_range()
#' @keywords
#' @export
#' @examples
#' Z_L(1E-7, omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10))

Z_L <- function(L, omega) {
  Z_L <- complex(real = 0, imaginary = (omega * L))
  return(Z_L)
}


#' Z_Q
#' 
#' Calculates the impedance of a complex phase element from its "Q" value, the phase angle (90/n) degrees,
#' and a vector of angular frequency values. Returns a complex vector.
#' 
#' @param Q the "Q" value of the constant phase element (equal to C when n = 1), with units of Siemens * seconds^n.
#' @param n the "n" value where the phase angle is 90/n degrees.
#' @param omega a vector of angular frequency values, which can be created with omega_range()
#' @keywords
#' @export
#' @examples 
#' Z_Q(1E-5, 0.8, omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10))

Z_Q <- function(Q, n, omega) {
  1/(Q * (complex(real = 0, imaginary = 1) * omega)^n)
}


#' Z_W
#' 
#' Calculates the impedance of an infinite length Warburg element from the Warburg coefficient sigma
#' and a vector of angular frequency values. Returns a complex vector.
#' 
#' @param sigma the Warburg coefficient, with units of Ohms * seconds^-1/2.
#' @param omega a vector of angular frequency values, which can be created with omega_range()
#' @keywords
#' @export
#' @examples 
#' Z_V(50, omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10))

Z_W <- function(sigma, omega) {
  j = complex(real = 0, imaginary = 1)
  (1 - j) * sigma * omega^-0.5
}


#' Z_FLW
#' 
#' Calculates the impedance of a finite length ("short") Warburg element from its Z0 value,
#' time constant tau and a vector of angular frequency values. Returns a complex vector.
#' 
#' @param Z0 the DC resistance associated with the FLW in Ohms.
#' @param tau the time constant of the FLW in seconds.
#' @param omega a vector of angular frequency values, which can be created with omega_range()
#' @keywords
#' @export
#' @examples 
#' Z_FLW(50, 1, omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10))

Z_FLW <- function(Z0, tau, omega) {
  j = complex(real = 0, imaginary = 1)
  Z0 * (j * omega * tau)^-0.5 * tanh((j * omega * tau)^0.5)
}


#' Z_FSW
#' 
#' Calculates the impedance of a finite space ("open") Warburg element from its Z0 value,
#' time constant tau and a vector of angular frequency values. Returns a complex vector.
#' 
#' @param Z0 the DC resistance associated with the FSW in Ohms.
#' @param tau the time constant of the FSW in seconds
#' @param omega a vector of angular frequency values, which can be created with omega_range()
#' @importFrom pracma coth
#' @keywords
#' @export
#' @examples 
#' Z_FLW(50, 1, omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10))

Z_FSW <- function(Z0, tau, omega) {
  j = complex(real = 0, imaginary = 1)
  Z0 * (j * omega * tau)^-0.5 * coth((j * omega * tau)^0.5)
}


#' Z_G
#' 
#' Calculates the impedance of an infinite length Gerischer element from its Z0 value,
#' rate constant k and a vector of angular frequency values. Returns a complex vector.
#' 
#' @param Z0 the DC resistance associated with the Gerischer element in Ohms
#' @param k the rate constant associated with the Gerischer element in seconds^-1
#' @param omega a vector of angular frequency values, which can be created with omega_range()
#' @keywords
#' @export
#' @examples 
#' Z_G(50, 1, omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10))

Z_G <- function(Z0, k, omega) {
  j = complex(real = 0, imaginary = 1)
  Z0 * (k + (j * omega))^-0.5
}


#' Z_FLG
#' 
#' Calculates the impedance of a finite length Gerischer element from its Z0 value,
#' rate constant k, time constant tau and a vector of angular frequency
#' values
#' 
#' @param Z0 the DC resistance associated with the FLG element in Ohms
#' @param k the rate constant associated with the FLG element in seconds^-1
#' @param tau the time constant associated with the FLG element in seconds
#' @param omega a vector of angular frequency values, which can be created with omega_range()
#' @keywords
#' @export
#' @examples 
#' Z_FLG(50, 100, 0.1, omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10))

Z_FLG <- function(Z0, k, tau, omega) {
  j = complex(real = 0, imaginary = 1)
  Z0 * (k + (j * omega * tau))^-0.5 * tanh((k + (j * omega * tau))^0.5)
}


#' trans_line
#' 
#' Calculates the impedance of a finite length transmission line where one rail has zero
#' impedance, the other parallel rail has resistance of R for each unit, where a component
#' U connects the two rails which is a complex vector of impedance values for any other
#' equivalent circuit, and the transmission line is n units long. The maximum length is 25
#' units.
#' 
#' @param R The resistance in one of the parallel rails per unit in Ohms.
#' @param U A complex vector (i.e., the result of any other impedance calculation)
#' @param n The number of units in the transmission line
#' @import magrittr
#' @keywords
#' @export
#' @examples 
#' trans_line(R = 2, U = Z_C(1E-6, omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10)), n  = 10) # this is equivalent to the FSW.

trans_line <- function(R, U, n) {
  require(magrittr)
  if(n >= 25) stop("Values for n higher than 25 cannot be calculated.")
  s <- lapply(1:n, function(n) {
    "ser(Z_R(R), par(U," # open 3, close 1 (every loop, open 2)
  }) %>%
    do.call(paste, .) %>%
    paste("ser(Z_R(R), U") %>% # open 2, close 1
    paste(., lapply(1:n, function(n) {
      "))"
    }) %>%
      do.call("paste", .)) %>%
    paste(., ")")
  
  spec <- eval(parse(text = s)) %>% as.complex
  return(spec)
}