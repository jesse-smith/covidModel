#' Cross-Validate the STL Decomposition Step of `prep_linelist()`
#'
#' `cv_linelist_decomposition()` applies rolling cross-validation to the
#' linelist decomposition. It repeatedly applies the STL decomposition step of
#' \code{\link[covidModel:prep_linelist]{prep_linelist()}} to each stable point
#' in the timeseries and obtains "forecast" errors of the portion of the smooth
#' conditional on future data. See
#' \code{\link[covidModel:cv_decomposition]{cv_decomposition()}}
#' for details.
#'
#' @inheritParams prep_linelist
#'
#' @return A list of `tibble` objects, each containing the results of one
#'   sampling step for the dates in `start_date + trend` to
#'   `end_date - trend/2`, where `end_date` is the last completely observed
#'   date. See the `Value` section of
#'   \code{\link[covidModel:validate_decomposition]{validate_decomposition()}}
#'   for information on the components of each sample.
#'
#' @seealso \code{\link[covidModel:prep_linelist]{prep_linelist()}},
#'   \code{\link[covidModel:cv_decomposition]{cv_decomposition()}}
#'
#' @keywords internal
#'
#' @export
cv_linelist_decomposition <- function(
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
    # Perform rolling cross-validation along time series
    cv_decomposition(
      .col = "observed_cleaned",
      trend = trend,
      period = period
    )
}

#' Cross-Validate Predictions in an STL Decomposition Conditional on Future Data
#'
#' `cv_decomposition()` applies rolling cross-validation to the portion of an
#' STL decomposition conditional on future data. It provides
#' the predicted values and associated "errors" for each "forecast" horizon and
#' decomposed component across all stably-estimable time points.
#'
#' When smoothing time series using LOESS, a portion of the smooth will change
#' as future data comes in (approximately the last `trend/2` data points). These
#' points are said to be conditional on future data, and the difference between
#' the smooth with and without future data can be computed for all time points
#' that have already reached the "stable" portion of the series. This is done by
#' rolling the decomposition step across the time series and comparing the
#' smoothed point at each "forecast" horizon to the stable estimates. In effect,
#' this is rolling cross-validation of the STL decomposition using the fully
#' smoothed data as the reference values.
#'
#' @inherit cv_linelist_decomposition params return
#'
#' @param .data A data frame containing the time series data to resample
#'
#' @param .col The column containing the data to resample
#'
#' @seealso The workhorse function
#'   \code{\link[covidModel:validate_decomposition]{validate_decomposition()}}
#'   and the higher-level function
#'   \code{
#'   \link[covidModel:cv_linelist_decomposition]{cv_linelist_decomposition()}
#'   }
#'
#' @keywords internal
#'
#' @export
cv_decomposition <- function(
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

  endpoints <- seq(start, end, by = 1L)

  rlang::inform("Calculating decomposition error...")

  purrr::map(
    endpoints,
    ~ validate_decomposition(
      .x,
      .data = .data,
      .ref = reference_data,
      period = period,
      trend = trend
    )
  )
}

#' Map a Dataset to an STL Decomposition Using Data Up to a Specific Time Point
#'
#' `validate_decomposition()` creates a sample decomposition and errors from a
#' dataset, a reference decomposition, and decomposition parameters. It is
#' designed to be used as a mapping function passed to
#' \code{\link[purrr:map]{map()}} within
#' \code{\link[covidModel:cv_decomposition]{cv_decomposition()}}.
#'
#' @inheritParams cv_decomposition
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
#'   \link[covidModel:cv_decomposition]{cv_decomposition()}
#'   }
#'
#' @keywords internal
#'
#' @export
validate_decomposition <- function(
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
