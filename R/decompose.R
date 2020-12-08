#' @export
decompose_time <- function(
  .data,
  .col,
  period = NULL,
  trend = NULL,
  method = c("bsts", "stl", "twitter"),
  trend_model = c("semilocal", "local", "robust", "level"),
  season_model = c("regression", "harmonic"),
  iterations = 11000,
  ...
) {

  method <- rlang::arg_match(method)[[1]]

  if (method == "bsts") {
    decompose_time_bsts(
      .data,
      .col = .col,
      .t = .t,
      trend = NULL,
      period = NULL,
      trend_model = trend_model,
      season_model = season_model,
      iterations = iterations,
      ...
    )
  } else if (method == "stl") {
    decompose_time_stl(
      .data,
      .col = .col,
      .t = .t,
      trend = trend,
      period = period,
      ...
    )
  } else if (method == "twitter") {
    decompose_time_twitter(
      .data,
      .col = .col,
      .t = .t,
      trend = trend,
      period = period,
      ...
    )
  }
}

#' @export
decompose_time_bsts <- function(
  .data,
  .col = NULL,
  .t = NULL,
  period = NULL,
  trend = NULL,
  trend_model = c("semilocal", "local", "robust", "level"),
  season_model = c("regression", "harmonic"),
  iterations = ceiling(1e4 / (1 - burn)),
  burn = 0.2,
  ...
) {

    bsts_time_decompose(
      .data,
      .col = .col,
      .t = .t,
      trend_model = trend_model,
      season_model = season_model,
      season_period = period,
      iterations = iterations,
      ...
    )

}

#' @export
decompose_time_stl <- function(
  .data,
  .col,
  period = NULL,
  trend = NULL,
  method = "stl",
  ...
) {

  method <- rlang::arg_match(method)

  if (rlang::is_null(period)) {
    period <- "auto"
  }

  if (rlang::is_null(trend)) {
    trend <- "auto"
  }

  .data %>%
    timetk::tk_tbl(rename_index = ".t", silent = TRUE) %>%
    anomalize::time_decompose(
      target = .col,
      method = method,
      frequency = period,
      trend = trend,
      message = FALSE,
      ...
    )
}

#' @export
decompose_time_twitter <- function(
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

#' @export
bsts_time_decompose <- function(
  .data,
  .col = NULL,
  .t = NULL,
  trend_model = c("semilocal", "local", "robust", "level"),
  season_model = c("regression", "harmonic"),
  season_period = NULL,
  iterations = ceiling(1e4 / (1 - burn)),
  burn = 0.2,
  ...
) {

  trend_model <- rlang::arg_match(trend_model)[[1]]

  season_model <- rlang::arg_match(season_model)[[1]]

  timetk::tk_zoo(.data, select = .col, date_var = .t) %>%
    {list(.data = .)} %>%
    bsts_trend(
      method = trend_model,
      ...
    ) %>%
    bsts_season(
      period = season_period,
      method = season_model,
      ...
    ) %>%
    bsts_fit(
      iterations = iterations,
      ...
    ) %>%
    decomp_bsts(burn = burn)
}
