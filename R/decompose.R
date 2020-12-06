decompose_time <- function(
  .data,
  .col,
  period,
  trend,
  method = c("bsts", "stl", "twitter"),
  ...
) {

  method <- rlang::arg_match(method)[[1]] %>%
    set_attr(which = "class")

  UseMethod("decompose_time", method)
}

decompose_time.bsts <- function(
  .data,
  .col = NULL,
  period = NULL,
  trend = NULL,
  method = "bsts"
) {
  y <- timetk::tk_zoo(
    .data,
    select = .col
  )
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
  trend_model = c("semilocal", "local", "robust", "level"),
  season_model = c("regression", "harmonic"),
  season_period = NULL,
  iterations = 11000,
  ...
) {

  trend_method <- rlang::arg_match(trend)[[1]]
  
  season_method <- rlang::arg_match(season)[[1]]

  timetk::tk_zoo(.data, select = .col, date_var = .t) %>%
    {list(.data = .)} %>%
    bsts_trend(
      method = trend_method,
      ...
    ) %>%
    bsts_season(
      period = season_period,
      method = season_method,
      ...
    ) %>%
    bsts_fit(
      iterations = iterations,
      ...
    )
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

#' Bayesian Structural Time Series: Seasonal Components
#'
#' `bsts_season()` is a generic that wraps the two seasonal models in the
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
#' @param period The length of a full seasonal cycle. Either a time-based definition (e.g. "1 week")
#'   or the number of observations in a season (e.g. 7). Supplying a
#'   number makes this equivalent to `nseasons` in
#'   \code{\link[bsts:AddSeasonal]{AddSeasonal()}} or `period` in
#'   \code{\link[bsts:AddTrig]{AddTrig()}} (for methods `regression` or
#'   `harmonic`, respectively).
#'
#' @param season The length of a season within a period (e.g. "1 month" for a 1 year period
#'   with monthly seasonality); the default is one season per
#'   observation within a period. Can be supplied in the same
#'   way as `period`. This parameter is ignored for
#'   `method = "harmonic"`.
bsts_season <- function(
  state = list(),
  y = state[["y"]],
  method = c("regression", "harmonic"),
  period = NULL,
  seasons = NULL,
  ...
) {
  method <- rlang::arg_match(method) %>%
    vec_slice(i = 1L) %>%
    set_attr(which = "class")

  UseMethod("bsts_seasonal", method)
}

bsts_season.regression <- function(
  state = list(),
  y = state[["y"]],
  method = "regression",
  period = NULL,
  season = NULL,
  sigma.prior = NULL,
  initial.state.prior = NULL,
  sdy = NULL
) {
  
  if (rlang::is_null(period)) {
    period <- "auto"
  }
  
  if (rlang::is_null(season)) {
    season <- "auto"
  }
  
  period <- timetk::tk_get_frequency(
    timetk::tk_index(y),
    period = period
  )
  
  period <- timetk::tk_get_frequency(
    timetk::tk_index(y),
    period = season
  )

  bsts::AddSeasonal(
    state.specification = state,
    y = y,
    nseasons = period,
    season.duration = season,
    sigma.prior = sigma.prior,
    initial.state.prior = initial.state.prior,
    sdy = sdy
  )
}

bsts_season.harmonic <- function(
  state = list(),
  y = state[["y"]],
  method = "harmonic",
  period = NULL,
  season = NULL,
  sigma.prior = NULL,
  initial.state.prior = NULL,
  sdy = NULL
) {

  if (rlang::is_null(period)) {
    period <- "auto"
  }
  
  if (!rlang::is_null(season)) {
    rlang::warn("`season` is ignored when `method = 'harmonic'`")
  }
  
  bsts::AddTrig(
    state.specification = inset2(list(), "y", NULL),
    y = y,
    period = period,t
    frequencies = 1L,
    sigma.prior = sigma.prior,
    sdy = sdy,
    method = "harmonic"
  )
}

bsts_fit <- function(
  state,
  y = state[["y"]],
  iterations = 11000,
  ...
) {
  bsts::bsts(
    state.specification = inset2(state, "y", NULL),
    formula = y,
    niter = iterations,
    ...
  )
}
