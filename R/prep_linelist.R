#' Prepare a Linelist for Rt Estimation
#'
#' `prep_linelist()` converts a linelist to an incidence curve, replaces
#' anomalous counts with the expected value, and decomposes the result into
#' trend, seasonality, and remainder components. It also filters out reporting
#' errors and truncates the data at the last fully observed incidence date
#' (as defined by `pct_reported`, see that parameter for details). All
#' calculations are performed on the log scale, but the result is returned on
#' the input scale (assumed linear).
#'
#' @inheritParams deanomalize
#'
#' @param .data A data frame containing one incident observation per row
#'
#' @param .collection_date `<tidy-select>` A `Date` column to use as the
#'   collection date of the observed case
#'
#' @param .report_date `<tidy-select>` A `Date` column to use as the report
#'   date of the observed case
#'
#' @param start_date The start date of the epidemic;
#'   defaults to `"2020-03-12"`, which is the beginning of the contiguous
#'   part of Shelby County's observed cases (at least one case observed per
#'   day since that date).
#'
#' @param delay_period The length of time to use in calculating reporting
#'   delay; can be a time-based definition (e.g. "2 weeks") or an integer number
#'   of days. If `NULL`, `delay_period` is set to `"14 days"`.
#'
#' @param pct_reported The percent of total cases reported before considering
#'   a collection date to be fully observed. It is not recommended to set this
#'   to `1`, as reporting delays typically contain very large outliers which
#'   will skew the results. The default is `0.9`, which strikes a balance
#'   between sensitivity and robustness in Shelby County data.
#'
#' @param plot_anomalies Should anomalies be plotted for visual inspection? If
#'   `TRUE`, the plot will be on the log-scale.
#'
#' @return A `tibble` with a date column (named the same as the column specified
#'   by `.collection_date`) and `observed`, `season`, `trend`, and `remainder`
#'   columns. All numeric columns have outlier replaced.
#'
#' @export
prep_linelist <- function(
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

  .data %>%
    # Clean and deanomalize
    prep_linelist_decomposition(
      .collection_date = collect_nm,
      .report_date = report_nm,
      start_date = start_date,
      trend = trend,
      period = period,
      delay_period = delay_period,
      pct_reported = pct_reported,
      cutoff = cutoff,
      plot_anomalies = plot_anomalies
    ) %T>%
    # Decompose
    {rlang::inform("Decomposing cleaned observations...")} %>%
    anomalize::time_decompose(
      target = "observed_cleaned",
      method = "stl",
      trend = if (is.null(trend)) "auto" else trend,
      frequency = if (is.null(period)) "auto" else period,
      message = TRUE
    ) %>%
    # Invert log transform using decomposed components
    expm1_decomposed()
}

#' Prepare Linelist for STL Decomposition of Incidence Curve
#'
#' `prep_linelist_decomposition()` prepares linelist data for decomposition
#' by \code{\link[anomalize:time_decompose]{time_decompose(method = "stl)}}. It
#' is the first step in \code{\link[covidModel:prep_linelist]{prep_linelist()}}.
#'
#' @inheritParams prep_linelist
#'
#' @return A `tibble` containing cleaned and deanomalized count data on the
#'   log-plus-1 scale, as well as a decomposition of that data and information
#'   associated with anomaly detection
#'
#' @seealso Component functions
#'   \code{\link[covidModel:clean_linelist]{clean_linelist()}} and
#'   \code{\link[covidModel:deanomalize]{deanomalize()}}, as well as
#'   higher-level function
#'   \code{\link[covidModel:prep_linelist]{prep_linelist()}}
#'
#' @export
prep_linelist_decomposition <- function(
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

  start_date <- lubridate::as_date(start_date)

  .data %>%
    clean_linelist(
      .collection_date = collect_nm,
      .report_date = report_nm,
      start_date = start_date,
      delay_period = delay_period,
      pct_reported = pct_reported
    ) %T>%
    {rlang::inform("Correcting anomalies...")} %>%
    # Convert to counts (on log scale)
    dplyr::count(.data[[collect_nm]]) %>%
    dplyr::mutate(n = log1p(.data[["n"]])) %>%
    # Replace anomalies
    deanomalize(
      .col = "n",
      trend = trend,
      period = period,
      cutoff = cutoff,
      plot = plot_anomalies,
      quiet = TRUE
    )
}

#' Remove Incomplete, Illogical, and Missing Data from an Incidence Linelist
#'
#' `clean_linelist()` creates a cleaned version of an incidence linelist with
#' collection and report dates. It removes observations with collection dates
#' that have not been fully reported (as determined by `pct_reported`), as well
#' as observations with missing collection dates or collection dates prior to
#' `start_date`. It then checks that all report dates are on or after the
#' corresponding collection dates and removes observations where this is untrue.
#'
#' @inheritParams prep_linelist
#'
#' @return A `tibble` containing the cleaned linelist
#'
#' @seealso Higher-level functions
#'   \code{
#'   \link[covidModel:prep_linelist_decomposition]{prep_linelist_decomposition()}
#'   }, \code{\link[covidModel:prep_linelist]{prep_linelist()}}
#'
#' @export
clean_linelist <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  start_date = "2020-03-12",
  delay_period = "14 days",
  pct_reported = 0.9
) {

  collect_expr <- rlang::enquo(.collection_date)

  report_expr <- rlang::enquo(.report_date)

  collect_nm <- coviData::select_colnames(.data, !!collect_expr)

  report_nm <- coviData::select_colnames(.data, !!report_expr)

  removing_start_date <- paste0(
    "Removing collection dates earlier than ", start_date
  )

  removing_illogical_or_missing <- paste0(
    "Removing logically inconsistent or missing observations"
  )

  start_date <- lubridate::as_date(start_date)

  cleaner_data <- .data %T>%
    {rlang::inform(removing_start_date)} %>%
    tidylog::filter(lubridate::as_date(start_date) <= .data[[collect_nm]]) %T>%
    {rlang::inform(removing_illogical_or_missing)} %>%
    tidylog::filter(
      !is.na(.data[[collect_nm]]),
      .data[[collect_nm]] <= .data[[report_nm]]
    )

  end_date <- estimate_delay(
    .data,
    .collection_date = collect_nm,
    .report_date = report_nm,
    pct = pct_reported,
    period = delay_period,
    rtn = "last_complete",
    min_dt = lubridate::as_date(start_date),
    quiet = TRUE
  ) %>%
    dplyr::pull(.data[[collect_nm]])

  removing_incomplete <- paste0(
    "Removing collection dates with incomplete data"
  )

  cleaner_data %T>%
    {rlang::inform(removing_incomplete)} %>%
    tidylog::filter(.data[[collect_nm]] <= end_date) %T>%
    {rlang::inform(paste("Last complete collection date is ", end_date, "\n"))}

}

#' Convert Decompositions Performed Under a `log1p()` Transform to Linear Scale
#'
#' `expm1_decomposed()` converts the output of
#' \code{\link[anomalize:time_decompose]{time_decompose()}} back to the linear
#' scale when the input to `time_decompose()` is on the log scale. This amounts
#' to selective sequential application of \code{\link[base:expm1]{expm1()}} to
#' the component columns.
#'
#' Because log-scale additive decomposition is multiplicative decomposition on
#' the linear scale, functions which automatically perform decomposition on the
#' log scale yield unexpected output. To remedy this, an approximate conversion
#' back to the linear scale can be performed. The steps are as follows (with all
#' exponentiation follow by an "add-1" operation):
#'
#' \enumerate{
#'   \item \strong{observed}: Exponentiate the `observed` column
#'   \item \strong{trend}: Exponentiate the `trend` column
#'   \item \strong{season}: Exponentiate `trend + season` and subtract the
#'     exponentiated `trend`
#'   \item \strong{remainder} Subtract the exponentiated `trend` and `season`
#'     from the exponentiated `observed` values (`observed - trend - season`)
#' }
#'
#' The underlying code is perhaps a simpler explanation than the above; consult
#' it for further exposition.
#'
#' Exponentiating the `observed` values is a simple inverse transform; however,
#' the component columns require some assumptions to convert back to a linear
#' scale. This process uses the `trend` as a "base" and simply exponentiates it
#' as in `observed`. This can then be used to derive the `season` component, and
#' the `remainder` is whatever is left over. While this is not a theoretically
#' valid process (there is none for this operation), it is consistent with
#' treating the `trend` as a smoothed version of `observed`, which is usually
#' done in practice. Once this assumption is made, the `season` transformation
#' becomes valid. The `remainder` is then assumed to be the difference between
#' the observed values (`observed`) expected values (`season + trend`), as
#' usual.
#'
#' @param .data The output of `time_decompose()`
#'
#' @return The input decomposition with `expm1()` applied appropriately
#'
#' @seealso \code{\link[anomalize:time_decompose]{time_decompose()}},
#'   \code{\link[base:expm1]{expm1()}}, calling function
#'   \code{\link[covidModel:prep_linelist]{prep_linelist()}}
#'
#' @export
expm1_decomposed <- function(.data) {

  .observed <- .data[["observed"]]
  .season <- .data[["season"]]
  .trend <- .data[["trend"]]
  .remainder <- .data[["remainder"]]

  dplyr::mutate(
    .data,
    observed = expm1(.observed),
    trend = expm1(.trend),
    season = expm1(.trend + .season) - .data[["trend"]],
    remainder = .data[["observed"]] - .data[["trend"]] - .data[["season"]]
  )
}
