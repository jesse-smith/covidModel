#' Resample the STL Decomposition Step of `prep_linelist()`
#'
#' `resample_linelist_decomposition()` repeatedly applies the STL decomposition
#' step of \code{\link[covidModel:prep_linelist]{prep_linelist()}} to obtain
#' "forecast" errors of the portion of the smooth conditional on future data.
#' See \code{\link[covidModel:resample_decomposition]{resample_decomposition()}}
#' for details.
#'
#' @inheritParams prep_linelist
#'
#' @return A list of `tibble` objects, each containing the results of one
#'   sampling step. See the `Value` section of
#'   \code{\link[covidModel:sample_decomposition]{sample_decomposition()}} for
#'   information on the components of each sample.
#'
#' @seealso \code{\link[covidModel:prep_linelist]{prep_linelist()}},
#'   \code{\link[covidModel:resample_decomposition]{resample_decomposition()}}
#'
#' @keywords internal
#'
#' @export
resample_linelist_decomposition <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  start_date = "2020-03-12",
  trend = "30 days",
  period = "7 days",
  delay_period = "14 days",
  pct_reported = 0.9,
  cutoff = 0.05,
  plot_anomalies = FALSE
) {
  collect_expr <- rlang::enquo(.collection_date)
  report_expr <- rlang::enquo(.report_date)

  collect_nm <- coviData::select_colnames(.data, !!collect_expr)
  report_nm <- coviData::select_colnames(.data, !!report_expr)

  # Clean and deanomalize
  prep_linelist_decomposition(
      .data,
      .collection_date = collect_nm,
      .report_date = report_nm,
      start_date = start_date,
      trend = trend,
      period = period,
      delay_period = delay_period,
      pct_reported = pct_reported,
      cutoff = cutoff,
      plot_anomalies = plot_anomalies
    ) %>%
    # Select needed columns
    dplyr::select(collect_nm, "observed_cleaned") %>%
    # Resample along time series
    resample_decomposition(
      .col = "observed_cleaned",
      trend = trend,
      period = period
    )
}

#' Resample Endpoint Errors in an STL Decomposition
#'
#' `resample_decomposition()` computes the right-hand endpoint errors of an
#' STL decomposition across all past data. Since smoothing generally depends on
#' both past and future observations, recent points in a smoothing algorithm
#' are subject to change conditional on future data (up to the limit of the
#' smoothing window, if one exists). `resample_decomposition()` provides the
#' previous changes for each "forecast" horizon, as well as the associated
#' error, for each component of the decomposition.
#'
#' @inherit resample_linelist_decomposition params return
#'
#' @param .data A data frame containing the time series data to resample
#'
#' @param .col The column containing the data to resample
#'
#' @seealso The workhorse function
#'   \code{\link[covidModel:sample_decomposition]{sample_decomposition()}} and
#'   the higher-level function
#'   \code{
#'   \link[covidModel:resample_linelist_decomposition]{
#'   resample_linelist_decomposition()
#'   }}
#'
#' @keywords internal
#'
#' @export
resample_decomposition <- function(
  .data,
  .col = "observed_cleaned",
  trend = "31 days",
  period = "7 days"
) {

  col_expr <- rlang::quo(.col)

  col_nm <- coviData::select_colnames(.data, !!col_expr)

  t_nm <- timetk::tk_get_timeseries_variables(.data)[[1L]]

  period <- timetk::tk_get_trend(
    .data[[t_nm]],
    period = if (is.null(period)) "auto" else period,
    message = FALSE
  )

  trend <- timetk::tk_get_trend(
    .data[[t_nm]],
    period = if (is.null(trend)) "auto" else trend,
    message = FALSE
  )

  # Create reference for comparison
  rlang::inform("Creating reference data...")
  reference_data <- anomalize::time_decompose(
    .data,
    target = col_nm,
    method = "stl",
    frequency = period,
    trend = trend,
    message = TRUE
  ) %>%
    expm1_decomposed()

  size <- vec_size(.data)

  half_trend <- as.integer(ceiling(trend / 2L) - 1L)

  start <- max(trend, period * 2 + 1L) %>% as.integer()

  end <- as.integer(size - half_trend)

  resamples <- seq(start, end, by = 1L)

  rlang::inform("Resampling decomposition...")

  purrr::map(
    resamples,
    ~ sample_decomposition(
      .x,
      .data = .data,
      .ref = reference_data,
      period = period,
      trend = trend
    )
  )
}

#' Map a Dataset to an STL Decomposition Using Data Up to a Specific TIme Point
#'
#' `sample_decomposition()` creates a sample decomposition and errors from a
#' dataset, a reference decomposition, and decomposition parameters. It is
#' designed to be used as a mapping function passed to
#' \code{\link[purrr::map]{map()}} within
#' \code{\link[covidModel:resample_decomposition]{resample_decomposition()}}.
#'
#' @inheritParams resample_decomposition
#'
#' @param .i The index of the sample; all data up to this point will be used
#'
#' @param .data The data to sample
#'
#' @param .ref A reference decomposition used to calculate errors
#'
#' @return A `tibble` containing the time index of the data, as well as columns
#'   `sample` (an indicator of the sample to which the data belongs), `horizon`,
#'   `observed`, `season`, `trend`, `remainder`, `season_error`, `trend_error`,
#'   and `remainder_error`
#'
#' @seealso \code{
#'   \link[covidModel:resample_decomposition]{resample_decomposition()}
#'   }
#'
#' @keywords internal
#'
#' @export
sample_decomposition <- function(
  .i,
  .data,
  .ref,
  period,
  trend
) {

  t_nm <- timetk::tk_get_timeseries_variables(.data)[[1L]]

  period <- timetk::tk_get_trend(
    .data[[t_nm]],
    period = if (is.null(period)) "auto" else period,
    message = FALSE
  )

  trend <- timetk::tk_get_trend(
    .data[[t_nm]],
    period = if (is.null(trend)) "auto" else trend,
    message = FALSE
  )

  half_trend <- as.integer(ceiling(trend / 2L) - 1L)

  start <- max(period * 2 + 1L, half_trend)

  .data %>%
    # Subset to the current window of data
    vec_slice(i = 1L:.i) %>%
    # Decompose
    anomalize::time_decompose(
      target = "observed_cleaned",
      method = "stl",
      frequency = period,
      trend = trend,
      message = FALSE
    ) %>%
    # Exponentiate
    expm1_decomposed() %>%
    # Calculate component error
    dplyr::mutate(
      season_error = .ref[["season"]] %>%
        vec_slice(i = seq_len(.i)) %>%
        subtract(.data[["season"]]) %>%
        {dplyr::if_else(dplyr::near(., 0), 0, .)},
      trend_error = .ref[["trend"]] %>%
        vec_slice(i = seq_len(.i)) %>%
        subtract(.data[["trend"]]) %>%
        {dplyr::if_else(dplyr::near(., 0), 0, .)},
      remainder_error = .ref[["remainder"]] %>%
        vec_slice(i = seq_len(.i)) %>%
        subtract(.data[["remainder"]]) %>%
        {dplyr::if_else(dplyr::near(., 0), 0, .)}
    ) %>%
    # Create sample indicator and "horizon" variable
    dplyr::mutate(
      sample = as.integer(.i),
      horizon = seq_len(vec_size(.)),
      .before = 1L
    ) %>%
    # Convert from tibbletime to tibble
    dplyr::as_tibble()
}
