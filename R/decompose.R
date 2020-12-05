decompose_time <- function(
  .data,
  .col,
  period,
  trend,
  method = c("bsts", "stl", "twitter"),
  ...
) {

  method <- rlang::arg_match(method) %>%
    vec_slice(i = 1) %>%
    set_attr(which = "class")

  UseMethod("decompose_time", method)
}

decompose_time.bsts <- function(
  .data,
  .col,
  period,
  trend,
  method = "bsts"
) {

}

decompose_time.stl <- function(
  .data,
  .col,
  period,
  trend,
  method = "stl"
) {

  method <- rlang::arg_match(method)

  .data %>%
    timetk::tk_tbl(rename_index = ".t") %>%
    anomalize::time_decompose(
      target = .col,
      method = method,
      frequency = period,
      trend = trend,
      ...
    )
}

decompose_time.twitter <- function(
  .data,
  .col,
  period,
  trend,
  method = "twitter"
) {

  method <- rlang::arg_match(method)

  .data %>%
    timetk::tk_tbl(rename_index = ".t") %>%
    anomalize::time_decompose(
      target = .col,
      method = method,
      frequency = period,
      trend = trend,
      ...
    )
}

bsts_time_decompose <- function(
  .data,
  .col = NULL,
  .t = NULL,
  period = "auto",
  trend = c("semilocal", "local", "robust", "level"),
  ...
) {

  trend <- rlang::arg_match(trend)[[1]]

  data <- timetk::tk_zoo(.data, select = .col, date_var = .t)
  remove(.data)

  period <- data %>%
    timetk::tk_index() %>%
    timetk::tk_get_frequency(period = period)

  bsts_trend(y = data, trend = trend) %>%



}

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
  y = state[["y"]],
  method = c("semilocal", "local", "robust", "level"),
  ...
) {
  method <- rlang::arg_match(method) %>%
    vec_slice(i = 1L) %>%
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

#' Bayesian Structural Time Series: Seasonal Components
#'
#' `bsts_seasonal()` is a generic that wraps the two seasonal models in the
#' bsts package into a user-friendly interface. Model-specific arguments are
#' passed via `...`; see the two methods (
#' \code{\link[bsts_seasonal.regression]{"regression"}} and
#' \code{\link[bsts_seasonal.harmonic]{"harmonic"}}
#' ) for details of those arguments. `bsts_seasonal()` also uses the timetk
#' package to make model specification easier or, if you prefer, automate it
#' entirely based on tunable heuristics.
#'
#' @inheritParams bsts_trend
#'
#' @param method Which seasonal model to use. Choose from `"regression"` or
#'   `"harmonic"`.
#'
#' @param period The length of a full seasonal cycle. Either "auto", a
#'   time-based definition (e.g. "1 week"), or the number of observations in a
#'   season (e.g. 7). Supplying a number makes this equivalent to `nseasons` in
#'   \code{\link[bsts:AddSeasonal]{AddSeasonal()}} or `period` in
#'   \code{\link[bsts:AddTrig]{AddTrig()}} (for methods `regression` or
#'   `harmonic`, respectively).
#'
#' @param seasons The number of seasons in a period (e.g. 12 for a 1 year period
#'   with monthly seasonality); the default is matched to the timescale of the
#'   supplied data. This parameter is ignored for `method = "harmonic"`.
bsts_seasonal <- function(
  state = list(),
  y = state[["y"]],
  method = c("regression", "harmonic"),
  period = "auto",
  seasons = "auto",
  ...
) {
  method <- rlang::arg_match(method) %>%
    vec_slice(i = 1L) %>%
    set_attr(which = "class")

  UseMethod("bsts_seasonal", method)
}

bsts_seasonal.regression <- function(
  state = list(),
  y = state[["y"]],
  method = "regression",
  period = "auto",
  seasons = "auto",
  sigma.prior = NULL,
  initial.state.prior = NULL,
  sdy = NULL
) {

  idx <- timetk::tk_index(y)

  bsts::AddSeasonal(
    state.specification = state,
    y = y,
    nseasons = timetk::tk_get_frequency(idx, period = period),
    season.duration = 1,
    sigma.prior = sigma.prior,
    initial.state.prior = initial.state.prior,
    sdy = sdy
  )
}

bsts_seasonal.harmonic <- function(
  state = list(),
  y = state[["y"]],
  method = "harmonic",
  sigma.prior = NULL,
  initial.state.prior = NULL,
  sdy = NULL
) {
  bsts::AddTrig(
    state.specification = inset2(list(), "y", NULL),
    y = y,
    period = period,
    frequencies = 1,
    sigma.prior = sigma.prior,
    sdy = sdy,
    method = "harmonic"
  )
}
