#' Bayesian Structural Time Series: Trend Components
#'
#' `bsts_trend()` is a generic that wraps the various trend models in the
#' bsts package into a user-friendly interface. Model-specific arguments are
#' passed via `...`; see the four methods (
#' \code{\link[bsts_trend.semilocal]{"semilocal"}},
#' \code{\link[bsts_trend.local]{"local"}},
#' \code{\link[bsts_trend.robust]{"robust"}}, and
#' \code{\link[bsts_trend.level]{"level"}}
#' ) for details of those arguments.
#'
#' @param state A list of state components you wish to add to. If omitted,
#'   an empty list will be assumed. This argument is named `state.specification`
#'   in bsts.
#'
#' @param y The time series to be modeled, as a numeric vector. Unlike bsts,
#'   this is piped forward as part of the state if defined, so you only need
#'   to specify it once (at the beginning of the model-building pipeline).
#'
#' @param method Which trend model to use. Choose from `"semilocal"`
#'   (the default), `"local"`, `"robust"`, or `"level"`.
#'
#' @param ... Additional arguments to pass to methods; see the methods above
#'   for details
#'
#' @return A list with the elements necessary to specify the chosen trend model
#'
#' @seealso \code{\link[bsts:bsts]{bsts()}},
#'  \code{\list[bsts:AddSemilocalLinearTrend]{AddSemilocalLinearTrend()}},
#'  \code{\list[bsts:AddLocalLinearTrend]{AddLocalLinearTrend()}},
#'  \code{\list[bsts:AddStudentLocalLinearTrend]{AddStudentLocalLinearTrend()}},
#'  \code{\list[bsts:AddLocalLevel]{AddLocalLevel()}}
#'
#' @export
bsts_trend <- function(
  state = list(),
  .data = state[[".data"]],
  method = c("semilocal", "local", "robust", "level"),
  ...
) {
  method <- rlang::arg_match(method)[[1]] %>%
    set_attr(which = "class")

  UseMethod("bsts_trend", method)

}

#' Semilocal Linear Trend State Component
#'
#' @inherit bsts::semilocal.linear.trend
#'
#' @export
bsts_trend.semilocal <- function(
  state = list(),
  y = state[["y"]],
  method = "semilocal",
  level.sigma.prior = NULL,
  slope.mean.prior = NULL,
  slope.ar1.prior = NULL,
  slope.sigma.prior = NULL,
  initial.level.prior = NULL,
  initial.slope.prior = NULL,
  sdy = NULL,
  initial.y = NULL
) {
  bsts::AddSemilocalLinearTrend(
    state.specification = state,
    y = y,
    level.sigma.prior = level.sigma.prior,
    slope.mean.prior = slope.mean.prior,
    slope.ar1.prior = slope.ar1.prior,
    slope.sigma.prior = slope.sigma.prior,
    initial.level.prior = initial.level.prior,
    initial.slope.prior = initial.slope.prior,
    sdy = sdy,
    initial.y = initial.y
  ) %>%
    inset2("y", y)
}

#' Local Linear Trend State Component
#'
#' @inherit bsts::add.local.linear.trend
#'
#' @export
bsts_trend.local <- function(
  state = list(),
  y = state[["y"]],
  method = "local",
  level.sigma.prior = NULL,
  slope.sigma.prior = NULL,
  initial.level.prior = NULL,
  initial.slope.prior = NULL,
  sdy = NULL,
  initial.y = NULL
) {
  bsts::AddLocalLinearTrend(
    state.specification = state,
    y = y,
    level.sigma.prior = level.sigma.prior,
    slope.sigma.prior = slope.sigma.prior,
    initial.level.prior = initial.level.prior,
    initial.slope.prior = initial.slope.prior,
    sdy = sdy,
    initial.y = initial.y
  ) %>%
    inset2("y", y)
}

#' Robust Local Linear Trend State Component
#'
#' @inherit bsts::add.student.local.linear.trend
#'
#' @export
bsts_trend.robust <- function(
  state = list(),
  y = state[["y"]],
  method = "robust",
  save.weights = FALSE,
  level.sigma.prior = NULL,
  level.nu.prior = NULL,
  slope.sigma.prior = NULL,
  slope.nu.prior = NULL,
  initial.level.prior = NULL,
  initial.slope.prior = NULL,
  sdy = NULL,
  initial.y = NULL
) {
  bsts::AddStudentLocalLinearTrend(
    state.specification = state,
    y,
    save.weights = save.weights,
    level.sigma.prior = level.sigma.prior,
    level.nu.prior = level.nu.prior,
    slope.sigma.prior = slope.sigma.prior,
    slope.nu.prior = slope.nu.prior,
    initial.level.prior = initial.level.prior,
    initial.slope.prior = initial.slope.prior,
    sdy = sdy,
    initial.y = initial.y
  ) %>%
    inset2("y", y)
}

#' Local Level Trend State Component
#'
#' @inherit bsts::add.local.level
#'
#' @export
bsts_trend.level <- function(
  state = list(),
  y,
  method = "level",
  sigma.prior = NULL,
  initial.state.prior = NULL,
  sdy = NULL,
  initial.y = NULL
) {
  bsts::AddLocalLevel(
    state.specification = state,
    y = y,
    sigma.prior = sigma.prior,
    initial.state.prior = initial.state.prior,
    sdy = sdy,
    initial.y = initial.y
  ) %>%
    inset2("y", y)
}