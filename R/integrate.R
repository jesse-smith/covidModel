#' Convert a Vector to a Continuous Function
#'
#' `vec_fun()` converts an input vector to a continuous, thrice-differentiable
#' function. `i` is the function domain; if left empty, the vector indices will
#' be used. Interpolation is performed using
#' \code{\link[stats:splinefun]{splinefun(method = "natural")}}; see that
#' function for details.
#'
#' `vec_fun()` is stricter than `splinefun()` in the accepted inputs; in
#' `vec_fun()`, `x` and `i` (if provided) must be cast-able to `double()` and
#' of the same length.
#'
#' The `ties` argument determines behavior when two values have the same (`i`, `x`)
#' coordinates. The default for `vec_fun()` is to assume the inputs are ordered
#' and proceed as usual; this is different than the default for `splinefun()`.
#'
#' @param x A vector to convert to a continuous function
#'
#' @param i An optional vector defining the input values for each output `x`
#'
#' @param ties `"ordered"`, a function that takes a single numeric vector and
#'   returns a single number, or a list of length 2 with components meeting the
#'   previous specifications. See the documentation for `ties` and "Details" in
#'   \code{\link[stats:approx]{approx()}} for further explanation.
#'
#' @param dev_interp Development argument - do not use
#'
#' @return A function with arguments `x` and `deriv` that returns the
#'   interpolated values of the input vectors (or the 1st - 3rd derivatives)
#'
#' @keywords internal
vec_fun <- function(
  y,
  x = NULL,
  ties = "ordered",
  approx = FALSE
) {

  # `splinefun()` isn't very strict or safe with its inputs; `vec_fun()` is
  # much stricter thanks to the vctrs framework
  y_vec <- vctrs::vec_cast(y, to = double())

  if (is.null(x)) {
    x_vec <- vctrs::vec_seq_along(y_vec)
  } else {
    x_vec <- vctrs::vec_cast(x, to = double())

    vctrs::vec_assert(
      x_vec,
      ptype = y_vec,
      size = vctrs::vec_size(y_vec),
      arg = "x"
    )
  }

  ties_is_ordered <- identical(ties, "ordered", ignore.environment = TRUE)

  ties_is_fun <- rlang::is_function(ties)

  ties_is_list_length_2 <- rlang::is_bare_list(ties, n = 2)

  if (ties_is_list_length_2) {
    ties %>%
      purrr::map_lgl(~ identical(.x, "ordered") | rlang::is_function(.x)) %>%
      all() ->
    ties_is_correct_list
  } else {
    ties_is_correct_list <- FALSE
  }

  assertthat::assert_that(
    any(ties_is_ordered, ties_is_fun, ties_is_correct_list),
    msg = paste0(
      "`ties` must be one of:\n\n",
      rlang::format_error_bullets(
        c(
          "\"ordered\" (the default)",
          "a function taking a numeric vector and returning a single number",
          paste0(
            "a list of length 2, ",
            "with elements that are either \"ordered\" or a function, as above"
          )
        )
      ),
      paste0(
        "\n\n",
        "See the `ties` documentation in ",
        "`?covidModel::vec_fun()` and `?stats::approx()` for details."
      )
    )
  )

  domain_min <- min(x_vec, na.rm = TRUE)

  domain_max <- max(x_vec, na.rm = TRUE)

  if (!rlang::is_true(approx)) {
    vec_fun <- stats::splinefun(
      x = x_vec,
      y = y_vec,
      method = "natural",
      ties = ties
    )
  } else {
    vec_fun <- stats::approxfun(
      x = x_vec,
      y = y_vec,
      method = "linear",
      ties = ties
    )
  }

  x_not_in_domain <- paste0(
    "`x` is outside the function domain of [",
    ceiling(domain_min * 100) / 100, ",",
    ceiling(domain_max * 100) / 100, "]; ",
    "return values will be linearly extrapolated using the slope at the ",
    "nearest in-domain value"
  )

  function(x, deriv = c(0L, 1L, 2L, 3L)) {

    deriv <- vctrs::vec_cast(deriv, to = integer()) %>% extract2(1)

    assertthat::assert_that(
      deriv %in% c(0L, 1L, 2L, 3L),
      msg = "`deriv` must be an integer between 0 and 3"
    )

    vctrs::vec_assert(deriv, size = 1L)

    x_in_domain <- domain_min <= x & x <= domain_max

    if (!all(x_in_domain)) {
      rlang::warn(x_not_in_domain, class = "warn_input_not_in_domain")
    }

    tryCatch(
      if (!approx) vec_fun(x, deriv = deriv) else vec_fun(x),
      error = function(e) rlang::cnd_entrace(e) %>% rlang::cnd_signal()
    )
  }
}

#' Convert a Time-Indexed Vector to Function of Time (i.e. x = f(t))
#'
#'
functionalize_vec <- function(x, t = NULL, ...) {

  # Types of `t` where `functionalize_vec()` works
  t_is_dt_dttm <- lubridate::is.Date(t) | lubridate::is.POSIXt(t)

  t_is_numeric <- any(
    vctrs::vec_is(t, ptype = integer()),
    vctrs::vec_is(t, ptype = double())
  )

  t_is_null <- is.null(t)

  if (t_is_dt_dttm) {
    t_as_num <- function(t) lubridate::decimal_date(t)
  } else if (t_is_numeric) {
    t_as_num <- function(t) t
  } else if (t_is_null) {
    t <- vctrs::vec_seq_along(x)
    t_as_num <- function(t) t
  } else {
    vctrs::stop_incompatible_cast(
      x = t,
      to = double(),
      x_arg = "t",
      to_arg = "double()",
      message = "`t` must be `NULL` or a date/datetime/numeric vector"
    )
  }

  f <- vec_fun(y = x, x = t_as_num(t), ties = "ordered", ...)

  t_min <- min(t, na.rm = TRUE)

  t_max <- max(t, na.rm = TRUE)

  t_not_in_domain <- paste0(
    "`t` is outside the function domain of [", t_min, ",", t_max, "]; ",
    "return values will be linearly extrapolated using the slope at the ",
    "nearest in-domain value"
  )

  function(t, deriv = c(0L, 1L, 2L, 3L)) {

    force(t_as_num)
    force(t_not_in_domain)
    force(f)

    t_not_in_domain_handler <- function(w) {
      w$message <- t_not_in_domain
      rlang::cnd_signal(w)
    }

    withCallingHandlers(
      f(x = t_as_num(t), deriv = deriv),
      warn_input_not_in_domain = t_not_in_domain_handler
    )
  }

}

functionalize <- function(.data, .x, .t = NULL, ...) {

  .x <- if (rlang::is_missing(.x)) rlang::sym("data") else rlang::ensym(.x)

  .data %>%
    timetk::tk_tbl(rename_index = ".t", silent = TRUE) %>%
    dplyr::rename(.x = !!.x) ->
  tbl

  remove(.data)

  .t_in_tbl <- ".t" %in% colnames(tbl)
  .t_null <- is.null(.t)

  tbl %<>% purrr::when(
      !.t_in_tbl & !.t_null ~ dplyr::rename(., .t = rlang::ensym(.t)),
      !.t_in_tbl & .t_null  ~ dplyr::mutate(., .t = vctrs::vec_seq_along(.)),
      ~ .
    ) %>%
    dplyr::select(.t, .x)

  f <- functionalize_vec(x = tbl$.x, t = tbl$.t, ...)

  attr(f, "data") <- tbl

  f
}

integrate_dt <- function(.data, .x, .t = NULL, slices = 1e3) {
  integrand <- functionalize(
    .data,
    .x = if (rlang::is_missing(.x)) rlang::missing_arg() else .x,
    .t = .t,
    approx = TRUE
  )

  .t_is_dt <- lubridate::is.Date(attr(integrand, "data")$.t)

  .t_is_dttm <- lubridate::is.POSIXt(attr(integrand, "data")$.t)

  .t_is_dt_dttm <- .t_is_dt | .t_is_dttm

  t_range <- range(attr(integrand, "data")$.t, na.rm = TRUE)

  if (.t_is_dt_dttm) {
    t_to_num <- function(t) lubridate::decimal_date(t)
  } else {
    t_to_num <- function(t) t
  }

  t_min <- t_range[[1]]

  t_max <- t_range[[2]]

  n_intervals <- slices * vctrs::vec_size(attr(integrand, "data"))

  t <- seq(t_min, t_max, length.out = n_intervals)

  dplyr::tibble(
    .t = t,
    data = ((t[[2]] - t[[1]]) / 2) * cumsum(integrand(t_to_num(t)))
  ) %>%
    # dplyr::mutate(data = data * (1 - 10/slices)) %>%
    functionalize() ->
  f

  attributes(f) <- NULL

  f
}
