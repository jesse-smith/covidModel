#' "Log(x + C)" Transform
#'
#' @param x A numeric vector to be transformed
#'
#' @param C A constant; defaults to square root of machine precision
#'
#' @export
logCp <- function(x, C = exp(1)) log1p(x + C - 1)

#' "Exp(x) - C" Transform
#'
#' @param x A numeric vector to be transformed
#'
#' @param C A constant; defaults to square root of machine precision
#'
#' @export
expmC <- function(x, C = exp(1)) exp(x) - C + 1

#' Convert Zeroes to Very Small Positive Number
#' @export
zero <- function(x, d_min = c(-Inf, 0)) {
  # Set domain max using machine double precision
  d_max <- sqrt(.Machine$double.eps)
  # Replace everything in domain with domain max
  x[d_min <= x & x < d_max] <- d_max
  # Return x with replaced values
  x
}

culogit <- function(x, min = 0, max = 9.38e5, x_rng) {
  x %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ tidyr::replace_na(.x, replace = 0) %>%
          cumsum() %>%
          {log1p((. - min) / (max - .))} %>%
          diff() %>%
          {c(NA, .)}
      )
    ) %>%
    tidyr::fill(tidyr::everything(), .direction = "up") ->
  x_t

  if (tibble::is_tibble(x)) x_t else x_t[[1]]
}

cuexpit <- function(x, min = 0, max = 9.38e5) {
  x %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ tidyr::replace_na(.x, replace = 0) %>%
          cumsum() %>%
          {(min + max * expm1(.)) / (1 + expm1(.))} %>%
          diff() %>%
          {c(NA, .)}
      )
    ) %>%
    tidyr::fill(tidyr::everything(), .direction = "up") ->
  x_t

  if (tibble::is_tibble(x)) x_t else x_t[[1]]
}

bc1p <- function(x, lambda = "auto") {
  forecast::BoxCox(x + 1, lambda = lambda)
}

bcm1 <- function(x, lambda = NULL) {
  lambda <- if (is.null(lambda)) attr(x, which = "lambda") else lambda

  assertthat::assert_that(!is.null(lambda), msg = "Argument 'lambda' is missing, with no default")

  forecast::InvBoxCox(x, lambda = lambda) - 1
}

softplus <- function(x, k = 1) {
  log1p(exp(k * x)) / k
}

inv_softplus <- function(x, k = 1) {
  log(expm1(k * x)) / x
}
