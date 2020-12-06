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
  .t = NULL,
  trend = NULL,
  period = NULL,
  method = "bsts",
  ...
) {
  
  .data %>%
    bsts_time_decompose(
      .col = .col,
      .t = .t,
      trend_model = trend_model,
      season_model = season_model,
      season_period = period,
      iterations = iterations,
      ...
    )
  
}

decompose_time.stl <- function(
  .data,
  .col,
  period,
  trend,
  method = "stl",
  ...
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

#' Bayesian Structural Time Series: Fit Model
#'
#' @inherit bsts::bsts
#' 
#' @export
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

prep_data 
