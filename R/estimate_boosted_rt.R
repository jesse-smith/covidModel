#' Boosted Bayesian Estimates of the Effective Reproduction Number
#'
#' `estimate_boosted_rt()` applies the approach of Cori et al (2013) to estimate
#' a gamma-distributed reproduction number. It implements additional
#' pre-processing to handle outliers and seasonality, and it smooths the data
#' \emph{before} computing Rt, rather than during the estimate. It also corrects
#' the portion of the smooth conditional on future data using rolling
#' cross-validation and geometrically-weighted bootstraps based on the residuals
#' for each future-conditional time point. Bootstrap weights are calculated
#' using a geometric sequence with the first-order autoregressive coefficient
#' as the discount factor.
#'
#' @inherit estimate_unboosted_rt params return
#'
#' @keywords internal
#'
#' @export
estimate_boosted_rt <- function(
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

  # Estimate with full dataset for reference
  reference <- suppressWarnings(
    suppressMessages(
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
        cutoff = cutoff
      )
    )
  )

  # Calculate number of days conditional on future data
  half_trend <- reference[[".t"]] %>%
    timetk::tk_get_trend(
      period = if (is.null(trend)) "auto" else trend,
      message = FALSE
    ) %>%
    divide_by(2L) %>%
    ceiling() %>%
    subtract(1L) %>%
    as.integer()

  # Perform resampling
  .data %>%
    cv_linelist_decomposition(
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
    {rlang::inform("Calculating Rt error...")} %>%
    purrr::map(
      ~ validate_rt(
        .x,
        .ref = reference,
        incid = "trend",
        .t = collect_nm,
        serial_interval_mean = serial_interval_mean,
        serial_interval_sd = serial_interval_sd,
        half_trend = half_trend
      )
    ) %T>%
    {rlang::inform("Integrating smoothing uncertainty...")} %>%
    reduce_rt_error() %>%
    weight_rt_error() %>%
    sample_rt_error() %>%
    integrate_rt_error(.ref = reference) %>%
    boost_rt(.ref = reference) %>%
    covidmodel_rt(serial_interval = attr(reference, "serial_interval"))
}

validate_rt <- function(
  .data,
  .ref,
  incid,
  .t,
  serial_interval_mean,
  serial_interval_sd,
  half_trend
) {

  t_quo <- rlang::enquo(.t)
  t_nm <- coviData::select_colnames(.data, !!t_quo)

  .data %>%
    dplyr::mutate(trend = pmax(.data[["trend"]], 0)) %>%
    calibrate_rt(
      incid = incid,
      .t = t_nm,
      serial_interval_mean = serial_interval_mean,
      serial_interval_sd = serial_interval_sd
    ) %>%
    # Subset to the "forecast" portion - points that depend on the future
    dplyr::slice_tail(n = half_trend) %>%
    dplyr::left_join(
      .ref,
      by = ".t",
      suffix = c("", "_ref")
    ) %>%
    dplyr::mutate(
      .incid_error = .data[[".incid_ref"]] - .data[[".incid"]],
      .pred_error = .data[[".pred_ref"]] - .data[[".pred"]],
      .pred_lower_error = .data[[".pred_lower_ref"]] - .data[[".pred_lower"]],
      .pred_upper_error = .data[[".pred_upper_ref"]] - .data[[".pred_upper"]],
      .mean_error = .data[[".mean_ref"]] - .data[[".mean"]],
      .cv_error = .data[[".cv_ref"]] - .data[[".cv"]]
    ) %>%
    dplyr::select(-dplyr::ends_with("_ref")) %>%
    # Add indicator for horizon
    dplyr::mutate(horizon = seq(1L, vec_size(.)), .before = 1L)
}

reduce_rt_error <- function(.data) {
  .data %>%
    purrr::reduce(
      ~ dplyr::bind_rows(
        dplyr::select(
          .x,
          .data[["horizon"]],
          .data[[".t"]],
          .data[[".mean_error"]],
          .data[[".cv_error"]]
        ),
        dplyr::select(
          .y,
          .data[["horizon"]],
          .data[[".t"]],
          .data[[".mean_error"]],
          .data[[".cv_error"]]
        )
      )
    ) %>%
    dplyr::arrange(.data[["horizon"]], .data[[".t"]]) %>%
    dplyr::group_by(.data[["horizon"]])
}

weight_rt_error <- function(.data) {
  base <- .data %>%
    dplyr::summarize(
      ar_mean = .data[[".mean_error"]] %>%
        stats::ar(
          order.max = 1L,
          aic = FALSE,
          method = "burg"
        ) %>%
        extract2("ar"),
      cv_mean = .data[[".cv_error"]] %>%
        stats::ar(
          order.max = 1L,
          aic = FALSE,
          method = "burg"
        ) %>%
        extract2("ar")
    )

  pwr <- .data[[".t"]] %>%
    vec_unique() %>%
    vec_seq_along() %>%
    rev()

  .data %>%
    dplyr::mutate(
      base_mean = base[["ar_mean"]] %>%
        vec_slice(i = .data[["horizon"]]),
      base_cv = base[["cv_mean"]] %>%
        vec_slice(i = .data[["horizon"]]),
      t_i = as.integer(.data[[".t"]] - min(.data[[".t"]]) + 1L),
      wt_mean = base_mean^pwr[t_i],
      wt_cv = base_cv^pwr[t_i],
      wt = wt_mean + wt_cv
    ) %>%
    dplyr::mutate(
      wt = wt / max(wt, na.rm = TRUE)
    ) %>%
    dplyr::select(-c("base_mean", "base_cv", "wt_mean", "wt_cv", "t_i"))
}

sample_rt_error <- function(.data, n = 1e4) {
  .data %>%
    dplyr::select(c("horizon", ".mean_error", ".cv_error", "wt")) %>%
    dplyr::slice_sample(n = n, weight_by = .data[["wt"]], replace = TRUE) %>%
    dplyr::select(c("horizon", ".mean_error", ".cv_error"))
}

integrate_rt_error <- function(.residuals, .ref, n = 1e3) {

  n_slice <- vec_unique_count(.residuals[["horizon"]])

  # There's a lot of select operations here - trying to keep memory usage down
  .ref %>%
    # Step 1: Subset `.ref` to "forecast" portion and only keep time, mean, cv
    dplyr::select(c(".t", ".mean", ".cv")) %>%
    dplyr::arrange(.data[[".t"]]) %>%
    dplyr::slice_tail(n = n_slice) %>%
    # Join the "forecast" portion of `.ref` by the forecast horizon
    dplyr::mutate(horizon = vec_seq_along(.)) %>%
    dplyr::left_join(.residuals, by = "horizon") %>%
    dplyr::ungroup() %>%
    dplyr::select(-"horizon") %>%
    # Group combined data by time point
    dplyr::group_by(.data[[".t"]]) %>%
    # Create resampled means, cvs, and standard deviations - get rid of rest
    dplyr::mutate(
      .mean_sample = .data[[".mean"]] + .data[[".mean_error"]],
      .cv_sample = .data[[".mean"]] + .data[[".cv_error"]],
      .sd_sample = .data[[".cv"]] * .data[[".mean"]]
    ) %>%
    dplyr::select(-c(".mean", ".cv", ".mean_error", ".cv_error")) %>%
    # Convert mean & sd to shape & rate
    dplyr::mutate(
      .shape_sample = gamma_shape(
        .data[[".mean_sample"]],
        .data[[".sd_sample"]]
      ),
      .rate_sample = gamma_rate(.data[[".mean_sample"]], .data[[".sd_sample"]])
    ) %>%
    # Sample from each distribution n times
    dplyr::summarize(
      .distr_sample = rgamma_vec(
        n = n,
        .data[[".shape_sample"]],
        .data[[".rate_sample"]]
      ) %>%
        as.matrix() %>%
        as.vector()
    )
}

boost_rt <- function(.data, .ref) {
  .data %>%
    # Compute summary statistics of "forecast" over full sample
    dplyr::summarize(
      .pred = stats::median(.data[[".distr_sample"]], na.rm = TRUE),
      .pred_lower = stats::quantile(
        .data[[".distr_sample"]],
        p = 0.025,
        type = 8,
        na.rm = TRUE
      ),
      .pred_upper = stats::quantile(
        .data[[".distr_sample"]],
        p = 0.975,
        type = 8,
        na.rm = TRUE
      ),
      .mean = mean(.data[[".distr_sample"]], na.rm = TRUE),
      .cv = stats::sd(.data[[".distr_sample"]], na.rm = TRUE) / .data[[".mean"]]
    ) %>%
    # Bind to stable estimates
    bind_boosted_rt(.ref = .ref)
}

bind_boosted_rt <- function(.data, .ref) {

  replace_obs <- vec_in(.ref[[".t"]], .data[[".t"]])

  .ref %>%
    dplyr::filter(!replace_obs) %>%
    dplyr::bind_rows(.data) %>%
    dplyr::arrange(.data[[".t"]])
}
