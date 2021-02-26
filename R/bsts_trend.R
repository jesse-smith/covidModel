
#' Bayesian Structural Time Series: Trend Components
#'
#' `bsts_trend()` is a generic that wraps the various trend models in the
#' bsts package into a user-friendly interface. Model-specific arguments are
#' passed via `...`; see the four methods for details of those arguments.
#'
#' @param state A list of state components you wish to add to. If omitted,
#'   an empty list will be assumed. This argument is named `state.specification`
#'   in bsts.
#'
#' @param .data The time series to be modeled, as a numeric vector. Unlike bsts,
#'   this is piped forward as part of the state if defined, so you only need
#'   to specify it once (at the beginning of the model-building pipeline).
#'
#' @param method Which trend model to use. Choose from `"semilocal"`
#'   (the default), `"local"`, `"robust"`, or `"level"`.
#'
#' @inheritParams bsts::AddSemilocalLinearTrend
#'
#' @inheritParams bsts::AddLocalLinearTrend
#'
#' @inheritParams bsts::AddStudentLocalLinearTrend
#'
#' @inheritParams bsts::AddLocalLevel
#'
#' @param ... Additional arguments to pass to methods; see the methods above
#'   for details
#'
#' @return A list with the elements necessary to specify the chosen trend model
#'
#' @seealso \code{\link[bsts:bsts]{bsts()}},
#'  \code{\link[bsts:AddSemilocalLinearTrend]{AddSemilocalLinearTrend()}},
#'  \code{\link[bsts:AddLocalLinearTrend]{AddLocalLinearTrend()}},
#'  \code{\link[bsts:AddStudentLocalLinearTrend]{AddStudentLocalLinearTrend()}},
#'  \code{\link[bsts:AddLocalLevel]{AddLocalLevel()}}
#'
#' @aliases bsts_trend.semilocal bsts_trend.local bsts_trend.robust
#'   bsts_trend.level
#'
#' @family bsts
#'
#' @export
bsts_trend <- function(
  state = list(),
  .data = state[[".data"]],
  method = c("semilocal", "local", "robust", "level"),
  ...
) {

  method <- rlang::arg_match(method)[[1L]]

  state <- as_bsts_trend(state, class = method)

  UseMethod("bsts_trend", state)

}

#' @rdname bsts_trend
#'
#' @export
bsts_trend.semilocal <- function(
  state = list(),
  .data = state[[".data"]],
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

  state.spec <- state %>%
    # validate_bsts_trend_semilocal() %>%
    set_class("list") %>%
    inset2(".data", NULL)

  bsts::AddSemilocalLinearTrend(
    state.specification = state.spec,
    y = .data,
    level.sigma.prior = level.sigma.prior,
    slope.mean.prior = slope.mean.prior,
    slope.ar1.prior = slope.ar1.prior,
    slope.sigma.prior = slope.sigma.prior,
    initial.level.prior = initial.level.prior,
    initial.slope.prior = initial.slope.prior,
    sdy = sdy,
    initial.y = initial.y
  ) %>%
    inset2(".data", .data)
}

#' @rdname bsts_trend
#'
#' @export
bsts_trend.local <- function(
  state = list(),
  .data = state[[".data"]],
  method = "local",
  level.sigma.prior = NULL,
  slope.sigma.prior = NULL,
  initial.level.prior = NULL,
  initial.slope.prior = NULL,
  sdy = NULL,
  initial.y = NULL
) {

  state.spec <- state %>%
    # validate_bsts_trend_local() %>%
    set_class("list") %>%
    inset2(".data", NULL)

  bsts::AddLocalLinearTrend(
    state.specification = state.spec,
    y = .data,
    level.sigma.prior = level.sigma.prior,
    slope.sigma.prior = slope.sigma.prior,
    initial.level.prior = initial.level.prior,
    initial.slope.prior = initial.slope.prior,
    sdy = sdy,
    initial.y = initial.y
  ) %>%
    inset2(".data", .data)
}

#' @rdname bsts_trend
#'
#' @export
bsts_trend.robust <- function(
  state = list(),
  .data = state[[".data"]],
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

  state.spec <- state %>%
    # validate_bsts_trend_robust() %>%
    set_class("list") %>%
    inset2(".data", NULL)

  bsts::AddStudentLocalLinearTrend(
    state.specification = state.spec,
    y = .data,
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
    inset2(".data", .data)
}

#' @rdname bsts_trend
#'
#' @export
bsts_trend.level <- function(
  state = list(),
  .data = state[[".data"]],
  method = "level",
  sigma.prior = NULL,
  initial.state.prior = NULL,
  sdy = NULL,
  initial.y = NULL
) {

  state.spec <- state %>%
    # validate_bsts_trend_level() %>%
    set_class("list") %>%
    inset2(".data", NULL)

  bsts::AddLocalLevel(
    state.specification = state.spec,
    y = .data,
    sigma.prior = sigma.prior,
    initial.state.prior = initial.state.prior,
    sdy = sdy,
    initial.y = initial.y
  ) %>%
    inset2(".data", .data)
}
