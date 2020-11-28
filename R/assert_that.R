#' Flexible Error Assertion with `assert_that()` and `abort()`
#'
#' This is an extension of \code{\link[assertthat:assert_that]{assert_that()}}
#' that allows rlang-like error specification.
#'
#' @inheritParams assertthat::assert_that
#'
#' @inheritParams rlang::abort
#'
#' @param data Additional data to be stored in the condition object
#'
#' @return `TRUE` if the assertions evaluate to `TRUE`, otherwise an error
#'   condition
#'
#' @keywords internal
assert <- function(
  ...,
  message = NULL,
  class = NULL,
  data = NULL,
  trace = NULL,
  parent = NULL
) {

  force(...)

  error_handler <- function(error) {
    if (is.null(message)) {
      message <- error$message
    }

    rlang::abort(
      message = message,
      class = class,
      !!!data,
      trace = trace,
      parent = parent
    )
  }

  tryCatch(
    assertthat::assert_that(...),
    error = error_handler
  )

  invisible(TRUE)
}
