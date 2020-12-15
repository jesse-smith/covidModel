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

  # Perform boosting
  .data %>%
    # Apply rolling cross-validation to incidence curve decomposition
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
    # Apply rolling cross-validation to Rt estimates of the above
    cv_rt(
      .ref = reference,
      .t = collect_nm,
      serial_interval_mean = serial_interval_mean,
      serial_interval_sd = serial_interval_sd,
      half_trend = half_trend
    ) %>%
    # Boostrap errors and update Rt distribution for future-conditional times
    boost_rt(.ref = reference) %>%
    # Convert to `covidmodel_rt` object
    covidmodel_rt(serial_interval = attr(reference, "serial_interval"))
}


#' Apply Rolling Cross-Validation to Rt Calculations Given Cross-Validated
#' Decomposition Estimates
#'
#' `cv_rt()` computes the reproduction number for each sample in the results
#' of \code{
#' \link[covidModel:cv_linelist_decomposition]{cv_linelist_decomposition()}
#' }.
#'
#' @param .data The cross-validation results from `cv_linelist_decomposition()`
#'
#' @param .ref Reference Rt
#'
#' @inheritParams calibrate_rt
#'
#' @param half_trend The portion of the trend conditional on future data
#'
#' @return A list of tibbles with Rt estimates and associated errors
#'
#' @family internal
#'
#' @export
cv_rt <- function(
  .data,
  .ref,
  .t,
  serial_interval_mean,
  serial_interval_sd,
  half_trend
) {
  .data %T>%
    {rlang::inform("Calculating Rt error...")} %>%
    purrr::map(
      ~ validate_rt(
        .x,
        .ref = .ref,
        incid = "trend",
        .t = .t,
        serial_interval_mean = serial_interval_mean,
        serial_interval_sd = serial_interval_sd,
        half_trend = half_trend
      )
    )
}

#' Boost Rt Estimates Given Reference Data and Cross-Validation Errors
#'
#' `boost_rt()` computes estimates of a Gamma-distributed reproduction number
#' given the results of rolling cross-validation of the decomposition and
#' the reference data.
#'
#' @param .data Results of cross-validation
#'
#' @param .ref Full estimates of Rt
#'
#' @return Rt estimates corrected for smoothing uncertainty
#'
#' @family internal
#'
#' @export
boost_rt <- function(.data, .ref) {
  .data %T>%
    {rlang::inform("Boosting expected value & uncertainty estimates...")} %>%
    # Reduce results to single tibble
    reduce_rt_error() %>%
    # Weight results by time and "forecast" horizon
    weight_rt_error() %>%
    # Sample results based on weights
    sample_rt_error() %>%
    # Integrate over samples by horizon
    integrate_rt_error(.ref = .ref) %>%
    # Compute summary statistics of updated distributions
    summarize_rt_error(.ref = .ref) %>%
    # Bind to stable estimates
    bind_boosted_rt(.ref = .ref)
}

#' Compute Errors for Rt Smoothing
#'
#'`validate_rt()` calculates Rt and "forecast" errors from a
#' a sample decomposition, a reference decomposition, and decomposition
#' parameters. It is designed to be used as a mapping function passed to
#' \code{\link[purrr:map]{map()}} within
#' \code{\link[covidModel:cv_decomposition]{cv_decomposition()}}.
#'
#' @inheritParams cv_rt
#'
#' @inheritParams calibrate_rt
#'
#' @return A tibble of rt estimates and associated errors in each summary
#'   statistic
#'
#' @family internal
#'
#' @export
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

#' Combine a List of Rt Errors into a Tibble Grouped by Forecast Horizon
#'
#' `reduce_rt_error()` combine a list of tibbles containing the mapped error
#' estimates using \code{\link[covidModel:validate_rt]{validate_rt()}}. Reduce
#' here is used in the context of "Map-Reduce"; it has nothing to do with
#' "reducing" the size of errors. This function is used internally by
#' \code{\link[covidModel:estimate_boosted_rt]{estimate_boosted_rt()}}.
#'
#' @param .data A list of tibbles containing error estimates for each time point
#'   in a time series
#'
#' @return A `tibble` containing error estimates by forecast horizon for each
#'   estimated time point
#'
#' @family internal
#'
#' @export
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

#' Calculate Geometrically Decaying Weights for Rt Sampling
#'
#' `weight_rt_error()` assigns weights according to a geometric decay curve
#' parameterized by the length of time from the current timepoint and the
#' first-order autoregressive coefficient, which is a measure of geometric
#' dependence in the absence of other modeling terms. This function is used
#' internally by \code{\link[covidModel:boost_rt]{boost_rt()}}.
#'
#' @param .data The result of `reduce_rt_error()`
#'
#' @return The input with a `wt` column containing sampling weights
#'
#' @family internal
#'
#' @export
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

#' Bootstrap Rt Estimates Using Past Errors
#'
#' `sample_rt_error()` performs weighted sampling of past errors in the mean
#' and coefficient of variation in Rt estimates using the weights from
#' \code{\link[covidModel:weight_rt_error]{weight_rt_error()}}. It converts
#' these estimates to shape and rate parameters for summary.
#'
#' @param .data The output of `weight_rt_error()`
#'
#' @param n The number of samples to take from each horizon
#'
#' @return A `tibble` of shape and rate estimates
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

#' Compute Updated Summary of Rt Smooth
#'
#' `summarize_rt_error()` computes Rt estimates based on the bootstrapping
#' performed in `sample_rt_error()`.
#'
#' @param .data The output of `sample_rt_error()`
#'
#' @param .ref Reference Rt data
#'
#' @return A `tibble` containing a forecast summary for each future-dependent
#'   data point, as given by \code{\link[covidModel:tidy_rt]{tidy_rt()}}
#'
#' @family internal
#'
#' @export
summarize_rt_error <- function(.data, .ref) {
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
    )
}

#' Bind Boosting Results to Stable Rt Estimates
#'
#' `bind_boosted_rt()` replaces the future-conditional data points in `.ref`
#' with the updated estimates in `.data`.
#'
#' @param .data The result of `summarize_rt_error()`
#'
#' @param .ref Reference Rt data
#'
#' @return Full updated Rt estimates
#'
#' @family internal
#'
#' @export
bind_boosted_rt <- function(.data, .ref) {

  replace_obs <- vec_in(.ref[[".t"]], .data[[".t"]])

  .ref %>%
    dplyr::filter(!replace_obs) %>%
    dplyr::bind_rows(.data) %>%
    dplyr::arrange(.data[[".t"]])
}
