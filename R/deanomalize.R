#' Replace Anomalies with Expected Values Using the `anomalize` Package
#'
#' `deanomalize()` wraps the workflow from the anomalize package into a single
#' function. This includes decomposition into seasonal, trend, and remainder
#' components using robust STL; identification of anomalies in the remainders
#' using an iterative Generalize Extreme Studentized Deviate Test; and
#' replacment of the identified anomalies with the seasonal + trend expected
#' value.
#'
#' @param .data A data frame containg a time-based column and a column of
#'   observations. The time-based column will be identified automatically. Can
#'   include other columns, but these will be dropped.
#'
#' @param .col The column containing the observations at each time point
#'
#' @param trend The length of time to use in trend decomposition; can be a
#'   time-based definition (e.g. "1 month") or an integer number of days. If
#'   `NULL` or `"auto"`, `trend` is set automatically using the tunable
#'   heuristics in the timetk package.
#'
#' @param period The length of time to use in seasonal decomposition; can be a
#'   time-based definition (e.g. "1 week") or an integer number of days. If
#'   `NULL` or `"auto"`, `period` is set automatically using the tunable
#'   heuristics in the timetk package.
#'
#' @param cutoff The cutoff value for anomaly detection; controls both the
#'   maximum percentage of data points that may be considered anomalies, as well
#'   as the critical value for the Generalized Extreme Studentized Deviate test
#'   used to detect the anomalies. Can be interpreted as the desired maximum
#'   probability that an individual data point is labeled an anomaly.
#'
#' @param quiet Should messages and warnings be suppressed?
#'
#' @param plot Should anomalies be plotted for visual inspection?
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[anomalize:time_decompose]{time_decompose(method = "stl")}}
#'
#' @return A `tibble` with a column for the time index and columns `observed`,
#'   `season`, `trend`, `remainder`, `remainder_l1`, `remainder_l2`, `anomaly`,
#'   and `observed_cleaned`
#'
#' @export
deanomalize <- function(
  .data,
  .col,
  period = NULL,
  trend = NULL,
  cutoff = 0.05,
  quiet = FALSE,
  plot = FALSE,
  ...
) {

  col_nm <- coviData::select_colnames(.data, .col)

  anomalize::time_decompose(
    .data,
    target = col_nm,
    method = "stl",
    frequency = if (is.null(period)) "auto" else period,
    trend = if (is.null(trend)) "auto" else trend,
    message = !quiet,
    ...
  ) %>%
    anomalize::anomalize(
      target = "remainder",
      method = "gesd",
      alpha = cutoff,
      max_anoms = cutoff,
      verbose = FALSE
    ) %T>%
    {if (plot) anomalize::plot_anomalies(.) %>% show()} %>%
    anomalize::clean_anomalies()
}
