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

#' Vectorized `rgamma()` Function
#'
#' `rgamma_vec()` allows vector inputs for `shape` and `rate`. Unlike
#' \code{\link[stats:rgamma]{rgamma()}}, it always returns a `tibble`, not a
#' vector, and does not accept a `scale` parameter (if you have a scale
#' parameter, use `1/scale` to get the rate). It also uses the notion of "size",
#' rather than "length", but this difference is not likely to affect end users.
#'
#' @param n Number of observations. If `vec_size(n) > 1L`, the size is
#'   taken to be the number required.
#'
#' @param shape,rate Shape and rate parameters for the gamma distribution.
#'   Both must be positive, `scale` strictly. Inputs of size 1 will be
#'   recycled, otherwise the sizes of `shape` and `scale` must match.
#'
#' @return A `tibble` of random deviates with `n` rows and columns corresponding
#'   to the values in the vectors supplied to `shape`, `rate` (named with the
#'   pattern `gamma{i}`, where `i` is the `i`th value in the recycled `shape`
#'   and `rate` vectors, padded by zeros to ensure consistent length).
#'
#' @seealso \code{\link[stats:rgamma]{rgamma()}}
#'
#' @keywords internal
#'
#' @export
rgamma_vec <- function(n, shape, rate = 1) {

  if (vec_size(n) > 1L) {
    n <- vec_size(n)
  } else {
    n <- vec_cast(n, to = integer())
  }

  shape <- vec_cast(shape, to = double())
  rate <- vec_cast(rate, to = double())

  params <- dplyr::tibble(shape = shape, rate = rate)

  n_digits <- params %>% vec_size() %>% stringr::str_length()

  padded_nums <- params %>%
    vec_seq_along() %>%
    stringr::str_pad(width = n_digits, pad = 0)

  col_names <- paste0("gamma", padded_nums)

  suppressMessages(
    purrr::pmap_dfc(params, ~ rgamma(n, shape = .x, rate = .y))
  ) %>%
    set_names(col_names)
}
