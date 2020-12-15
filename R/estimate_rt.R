#' (Boosted) Bayesian Estimates of Effective Reproduction Number
#'
#' `estimate_rt()` applies the approach of Cori et al (2013) to estimate
#' a gamma-distributed reproduction number. It implements additional
#' pre-processing to handle outliers and seasonality, and it smooths the data
#' \emph{before} computing Rt, rather than during the estimate. By default,
#' it also corrects the portion of the smooth conditional on future data using
#' rolling cross-validation and geometrically-weighted bootstraps based on the
#' residuals for each future-conditional time point. Bootstrap weights are
#' calculated using a geometric sequence with the first-order autoregressive
#' coefficient as the discount factor.
#'
#' @inherit estimate_unboosted_rt params return
#'
#' @param boost Should the error in future-conditional estimates be estimated
#'   and accounted for?
#'
#' @export
estimate_rt <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  serial_interval_mean = 6,
  serial_interval_sd = 4.17,
  start_date = "2020-03-12",
  trend = "30 days",
  period = "7 days",
  delay_period = "14 days",
  pct_reported = 0.9,
  cutoff = 0.05,
  plot_anomalies = FALSE,
  boost = TRUE
) {

  collect_quo <- rlang::enquo(.collection_date)
  report_quo <- rlang::enquo(.report_date)

  collect_nm <- coviData::select_colnames(.data, !!collect_quo)
  report_nm <- coviData::select_colnames(.data, !!report_quo)

  if (boost) {
    estimate_boosted_rt(
      .data,
      .collection_date = collect_nm,
      .report_date = report_nm,
      serial_interval_mean = serial_interval_mean,
      serial_interval_sd = serial_interval_sd,
      start_date = start_date,
      trend = trend,
      period = period,
      delay_period = delay_period,
      pct_reported = pct_reported,
      cutoff = cutoff,
      plot_anomalies = plot_anomalies
    )
  } else {
    estimate_unboosted_rt(
      .data,
      .collection_date = collect_nm,
      .report_date = report_nm,
      serial_interval_mean = serial_interval_mean,
      serial_interval_sd = serial_interval_sd,
      start_date = start_date,
      trend = trend,
      period = period,
      delay_period = delay_period,
      pct_reported = pct_reported,
      cutoff = cutoff,
      plot_anomalies = plot_anomalies
    )
  }
}
