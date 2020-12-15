rt_from_linelist <- function(
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
  resample = TRUE
) {

  collect_quo <- rlang::enquo(.collection_date)
  report_quo <- rlang::enquo(.report_date)

  collect_nm <- coviData::select_colnames(.data, !!collect_quo)
  report_nm <- coviData::select_colnames(.data, !!report_quo)

  if (resample) {
    resample_rt(
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
    run_rt_pipeline(
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

#' Run the Full Reproduction Number Modeling Pipeline
#'
#' `run_rt_pipeline()` is a wrapper around
#' \code{\link[covidModel:prep_linelist]{prep_linelist()}} and
#' \code{\link[covidModel:estimate_rt]{estimate_rt()}}. It exists purely for
#' convenience; see those functions for details.
#'
#' @inheritParams prep_linelist
#'
#' @inherit estimate_rt params return
#'
#' @seealso \code{\link[covidModel:prep_linelist]{prep_linelist()}},
#'   \code{\link[covidModel:estimate_rt]{estimate_rt()}}
#'
#' @export
run_rt_pipeline <- function(
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
  plot_anomalies = FALSE
) {

  collect_expr <- rlang::enquo(.collection_date)
  report_expr <- rlang::enquo(.report_date)

  collect_nm <- coviData::select_colnames(.data, !!collect_expr)
  report_nm <- coviData::select_colnames(.data, !!report_expr)

  .data %>%
    prep_linelist(
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
    estimate_rt(
      incid = "trend",
      .t = collect_nm,
      serial_interval_mean = serial_interval_mean,
      serial_interval_sd = serial_interval_sd
    )
}

#' Simple Bayesian Estimates of the Time-Varying Reproduction Number
#'
#' `estimate_rt()` uses the methodology of Cori et al 2013 to estimate the time-
#' varying reproduction number. It calls
#' \code{\link[EpiEstim:estimate_R]{estimate_R(method = "parametric_si")}} under
#' the hood. It purposefully does not support aggregating counts over multiple
#' time periods due to poor coverage of the resulting credible intervals, as
#' well as shifting of the results due to lagging of the results. Use
#' `smooth_for_rt()` to pre-smooth the data instead.
#'
#' @param .data A data frame containing the incidence curve and dates
#'
#' @param incid The quoted name of a numeric column containing the incidence
#'   curve
#'
#' @param t The quoted name of a date column corresponding to the observations
#'   in `incid`
#'
#' @param serial_interval_mean The average number of days between infection of a
#'   primary case and a secondary case
#'
#' @param serial_interval_sd The standard deviation of the number of days
#'   between infection of a primary case and a secondary case
#'
#' @return A `tibble` with columns `.t`, `.pred` (the median), `.pred_lower`
#'   (the lower bound of the 95% credible interval), `.pred_upper`
#'   (the upper bound of the 95% credible interval), `.mean` (the average), and
#'   `.cv` (the coefficient of variation)
#'
#' @export
estimate_rt <- function(
  .data,
  incid = "trend",
  .t = "collection_date",
  serial_interval_mean = 6,
  serial_interval_sd = 4.17
) {

  EpiEstim::estimate_R(
    incid = prep_data_rt(
      .data,
      .incid = incid,
      .t = .t
    ),
    method = "parametric_si",
    config = prep_config_rt(
      .data,
      serial_interval_mean = serial_interval_mean,
      serial_interval_sd = serial_interval_sd
    )
  ) %>%
    tidy_rt()
}

#' Prepare Data for Rt Calculation
#'
#' `prep_data_rt()` is an internal function used to prepare the `incid` argument
#' for \code{\link[EpiEstim:estimate_R]{estimate_R()}}.
#'
#' @inheritParams estimate_rt
#'
#' @param incid The quoted name of a numeric column containing the incidence
#'   curve
#'
#' @param t The quoted name of a date column corresponding to the observations
#'   in `incid`
#'
#' @keywords internal
prep_data_rt <- function(
  .data,
  .incid = NULL,
  .t = NULL
) {
  if (is.data.frame(.data)) {
    dplyr::select(
      .data,
      dates = .t,
      I = .incid
    )
  } else {
    .data %>%
      timetk::tk_tbl(rename_index = "dates") %>%
      dplyr::rename(I = .data[["x"]])
  }
}

prep_config_rt <- function(
  .data,
  period = 1L,
  serial_interval_mean = 6,
  serial_interval_sd = 4.17
) {

  t_start <- .data %>%
    vec_seq_along() %>%
    subset(2 <= .) %>%
    subset(. <= (max(.) - period + 1L))

  EpiEstim::make_config(
    t_start = t_start,
    t_end = t_start + period - 1L,
    mean_si = serial_interval_mean,
    std_si = serial_interval_sd
  )
}

#' Tidy an `estimate_R` Object
#'
#' `tidy_rt()` converts an `estimate_R` object from the EpiEstim package to the
#' `tibble` subclass `covidmodel_rt` with a `serial_interval` attribute. Works
#' when `estimate_R()` is called with `method = "parametric_si"`.
#'
#' @param rt An `estimate_R` object
#'
#' @return A `covidmodel_rt` object
#'
#' @export
tidy_rt <- function(rt) {

  si <- rt[["SI.Moments"]] %>%
    as.double() %>%
    set_names(colnames(rt[["SI.Moments"]]))

  rt[["R"]] %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      .t = vec_slice(rt[["dates"]], i = t_end)
    ) %>%
    dplyr::transmute(
      .t,
      .incid = rt[["I"]] %>% vec_slice(i = 1:(vec_size(.) - 1L)),
      .pred = `Median(R)`,
      .pred_lower = `Quantile.0.025(R)`,
      .pred_upper = `Quantile.0.975(R)`,
      .mean = `Mean(R)`,
      .cv = `Std(R)` / `Mean(R)`
    ) %>%
    covidmodel_rt(serial_interval = si)
}
