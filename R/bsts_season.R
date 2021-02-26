#' Bayesian Structural Time Series: Seasonal Components
#'
#' `bsts_season()` is a generic that wraps the two seasonal models in the
#' bsts package into a user-friendly interface. Model-specific arguments are
#' passed via `...`; see the two methods for details of those arguments.
#' `bsts_seasonal()` also uses the timetk package to make model specification
#' easier or, if you prefer, automate it entirely based on tunable heuristics.
#'
#' @inheritParams bsts_trend
#'
#' @param method Which seasonal model to use. Choose from `"regression"` or
#'   `"harmonic"`; `"dynamic"` will be implemented in the future.
#'
#' @param period The length of a full seasonal cycle. Either a time-based definition (e.g. "1 week")
#'   or the number of observations in a season (e.g. 7). Supplying a
#'   number makes this equivalent to `nseasons` in
#'   \code{\link[bsts:AddSeasonal]{AddSeasonal()}} or `period` in
#'   \code{\link[bsts:AddTrig]{AddTrig()}} (for methods `regression` or
#'   `harmonic`, respectively).
#'
#' @param season The length of a season within a period (e.g. "1 month" for a 1
#'   year period with monthly seasonality); the default is one season per
#'   observation within a period. Can be supplied in the same
#'   way as `period`. This parameter is ignored for
#'   `method = "harmonic"`.
#'
#' @param sigma.prior An object created by \code{\link[Boom:SdPrior]{SdPrior()}}
#'   describing the prior distribution for the standard deviation of the random
#'   walk increments
#'
#' @param initial.state.prior An object created by
#'   \code{\link[Boom:NormalPrior]{NormalPrior()}} describing the prior
#'   distribution of the the initial state vector (at time 1)
#'
#' @param sdy The standard deviation of the series to be modeled. This will be
#'   ignored if y is provided, or if all the required prior distributions are
#'   supplied directly
#'
#' @return A list with the updated state specification of a bsts model
#'
#' @aliases bsts_season.regression bsts_season.harmonic
#'
#' @family bsts
#'
#' @export
bsts_season <- function(
  state = list(),
  .data = state[[".data"]],
  method = c("regression", "harmonic", "dynamic"),
  period = NULL,
  season = NULL,
  sigma.prior = NULL,
  initial.state.prior = NULL,
  sdy = NULL
) {

  method <- rlang::arg_match(method)[[1L]]

  state <- as_bsts_season(state, class = method)

  UseMethod("bsts_season", state)
}

#' @rdname bsts_season
#'
#' @export
bsts_season.regression <- function(
  state = list(),
  .data = state[[".data"]],
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
    season <- 1L
  }

  period <- timetk::tk_get_frequency(
    timetk::tk_index(.data),
    period = period
  )

  season <- timetk::tk_get_frequency(
    timetk::tk_index(.data),
    period = season
  )

  state.spec <- state %>%
    # validate_bsts_season_regression() %>%
    set_class("list") %>%
    inset2(".data", NULL)

  bsts::AddSeasonal(
    state.specification = state.spec,
    y = .data,
    nseasons = period,
    season.duration = season,
    sigma.prior = sigma.prior,
    initial.state.prior = initial.state.prior,
    sdy = sdy
  ) %>%
    inset2(".data", .data)
}

#' @rdname bsts_season
#'
#' @export
bsts_season.harmonic <- function(
  state = list(),
  .data = state[[".data"]],
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

  state.spec <- state %>%
    # validate_bsts_season_harmonic() %>%
    set_class("list") %>%
    inset2(".data", NULL)


  bsts::AddTrig(
    state.specification = state.spec,
    y = .data,
    period = period,
    frequencies = 1L,
    sigma.prior = sigma.prior,
    sdy = sdy,
    method = "harmonic"
  ) %>%
    inset2(".data", .data)
}

#' Unfinished
# bsts_season.dynamic <- function(
#   state = list(),
#   .data = state[[".data"]],
#   method = "dynamic",
#   period = NULL,
#   season = NULL,
#   ar = TRUE,
#   sigma.prior = NULL,
#   na.action = na.exclude,
#   ...
# ) {
#
#   ar <- rlang::arg_match(ar, values = c(TRUE, FALSE))
#
#   bsts::AddDynamicRegression(
#     state.specification = inset2(state, ".data", NULL),
#     formula = .data,
#     data = .data,
#     model.options = coef_model,
#     na.action = na.action,
#     ...
#   ) %>%
#     inset2(".data", .data)
# }
