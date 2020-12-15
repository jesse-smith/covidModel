#' Convert Between Mean/SD and Shape/Rate of Gamma Distribution
#'
#' These functions return the mean, standard deviation, shape, or rate of a
#' gamma distribution given the other pair of parameters.
#'
#'
#' @param mean A vector of one or more means
#'
#' @param sd A vector of one or more standard deviations
#'
#' @param shape A vector of one or more shape parameters
#'
#' @param rate A vector of one or more rate parameters
#'
#' @name gamma-distr-conversion
#'
#' @aliases gamma_shape gamma_rate gamma_mean gamma_sd
#'
#' @keywords internal
NULL

#' @rdname gamma-distr-conversion
gamma_shape <- function(mean, sd) {
  (mean / sd)^2L
}

#' @rdname gamma-distr-conversion
gamma_rate <- function(mean, sd) {
  mean / sd^2L
}

#' @rdname gamma-distr-conversion
gamma_mean <- function(shape, rate) {
  shape / rate
}

#' @rdname gamma-distr-conversion
gamma_sd <- function(shape, rate) {
  sqrt(shape) / rate
}
