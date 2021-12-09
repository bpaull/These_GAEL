## Bettega Paul
## 2021_06_02 ==> 2021_06_02
## Function related to CRRA utility function

#' CRRA utility functoin
#' 
#' This is an R implementation of the CRRA function as define by 
#' Wakker, P. P. (2008). Explaining the characteristics of the power 
#' (CRRA) utility family. Health economics, 17(12), 1329-1344.
#'
#' @param x numeric vector (monetary amount)
#' @param r numeric vector (parameter of the utility function)
#'
#' @return numeric vector
#' @export
#'
#' @examples
crra <- function(x, r) {
  if (r > 0) return( x^r)
  if (r < 0) return(-x^r)
  if (r == 0) return(log(x))
}

#' Compute utility for a NART task
#' 
#' Compute the utility value associate with the choice of a specific value in 
#' a Numerical Analogue Risk Task.
#'
#' @param n integer vector of possible choice in the task
#' @param u utility function which take as first argument a monetary amount
#' @param ... additional arguments to be passed to u
#'
#' @return numeric vector
#' @export
#'
#' @examples
nart <- function(n, u, ...) {
  (64 - n)/64 * u(n, ...)
}

#' Find optimal value in a NART 
#' 
#' Find the optimal reponse in a NART for a given utility function.
#'
#' @param r parameter of the utility function
#' @param u utility function
#' @param interval vector of min an max for the NART
#'
#' @return
#' @export
#'
#' @examples
n_optim <- function(r, u, interval = c(0,64)) {
  r_space <- seq.int(interval[1], interval[2], 1)
  r_space[which.max(nart(r_space, u, r))]
}

#' Certainty equivalent for NART task with CRRA function
#'
#' @param n integer value in the NART respones space
#' @param r numeric parameter of the CRRA function
#'
#' @return
#' @export
#'
#' @examples
equivalent_certain.crra <- function(n, r) {
  exp((log(64 - n) - log(64) + r * log(n))/r)
}


#' Title
#'
#' @param r numeric parameter of the CRRA function
#' @param N vector of integer corresponding to choice in a NART task
#'
#' @return
#' @export
#'
#' @examples
eq_ir_dist.crra <- function(r, N) {
  sum(equivalent_certain.crra(n_optim(r, crra), r) - equivalent_certain.crra(N, r))
}