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