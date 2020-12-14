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
  t = "collection_date",
  serial_interval_mean = 6,
  serial_interval_sd = 4.17
) {

  EpiEstim::estimate_R(
    incid = prep_data_rt(
      .data,
      .incid = incid,
      .t = t
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
#' @param trend The length of time to use in trend decomposition; can be a
#'   time-based definition (e.g. "1 month") or an integer number of days. The
#'   default is `"30 days"`; however, if `NULL` or `"auto"`, `trend` is set
#'   automatically using the tunable heuristics in the timetk package.
#'
#' @param period The length of time to use in seasonal decomposition; can be a
#'   time-based definition (e.g. "1 week") or an integer number of days. The
#'   default is `"7 days"`; however, if `NULL` or `"auto"`, `period` is set
#'   automatically using the tunable heuristics in the timetk package.
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
#' @param cutoff The cutoff value for outlier detection; controls both the
#'   maximum percentage of data points that may be considered outliers, as well
#'   as the critical value for the Generalized Extreme Studentized Deviate test
#'   used to detect the outliers. Can be interpreted as the desired maximum
#'   likelihood that an individual data point is labelled an outlier.
#'
#' @param plot_anomalies Should anomalies be plotted for visual inspection? If
#'   `TRUE`, the plot will be on the log-scale.
#'
#' @return A `tibble` with a date column (named the same as the column specified
#'   by `.collection_date`) and `observed`, `season`, `trend`, and `remainder`
#'   columns. All numeric columns have outlier replaced.
prep_linelist <- function(
  .data,
  .collection_date = collection_date,
  .report_date = report_date,
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

  end_date <- estimate_delay(
    .data,
    .collection_date = collect_nm,
    .report_date = report_nm,
    pct = pct_reported,
    period = delay_period,
    rtn = "last_complete",
    min_dt = lubridate::as_date(start_date)
  ) %>%
    dplyr::pull(.data[[collect_nm]])

  rlang::inform(paste("Last complete collection date is ", end_date, "\n"))

  removing_start_date <- paste0(
    "`estimate_rt`: Removing collection dates earlier than ", start_date
  )

  removing_incomplete <- paste0(
    "`estimate_rt`: Removing collection dates with incomplete data"
  )

  removing_illogical_or_missing <- paste0(
    "`estimate_rt`: Removing logically inconsistent or missing observations"
  )

  start_date <- lubridate::as_date(start_date)

  .data %T>%
    {rlang::inform(removing_start_date)} %>%
    tidylog::filter(lubridate::as_date(start_date) <= .data[[collect_nm]]) %T>%
    {rlang::inform(removing_incomplete)} %>%
    tidylog::filter(.data[[collect_nm]] <= end_date) %T>%
    {rlang::inform(removing_illogical_or_missing)} %>%
    tidylog::filter(
      !is.na(.data[[collect_nm]]),
      .data[[collect_nm]] <= .data[[report_nm]]
    ) %T>%
    {rlang::inform("Correcting anomalies...")} %>%
    dplyr::count(.data[[collect_nm]]) %>%
    dplyr::mutate(n = log1p(.data[["n"]])) %>%
    deanomalize(
      .col = "n",
      trend = trend,
      period = period,
      cutoff = cutoff,
      plot = plot_anomalies
    ) %>%
    anomalize::time_decompose(
      target = "observed_cleaned",
      method = "stl",
      trend = if (is.null(trend)) "auto" else trend,
      frequency = if (is.null(period)) "auto" else period,
      message = FALSE
    ) %>%
    expm1_decomposed()
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
#'   \code{\link[base:expm1]{expm1()}}
#'
#' @keywords internal
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

#' Convert Between Mean/SD and Shape/Rate of Gamma Distribution
#'
#' These functions return the mean, standard deviation, shape, or rate of a
#' gamma distribution given the other pair of parameters.
#'
#'
#' @param mean A vector of one or more means
#'
#' @param sd A vector of one or more standard deviations
#'
#' @param shape A vector of one or more shape parameters
#'
#' @param rate A vector of one or more rate parameters
#'
#' @name gamma-distr-conversion
#'
#' @aliases gamma_shape gamma_rate gamma_mean gamma_sd
#'
#' @keywords internal
NULL

#' @rdname gamma-distr-conversion
gamma_shape <- function(mean, sd) {
  (mean / sd)^2L
}

#' @rdname gamma-distr-conversion
gamma_rate <- function(mean, sd) {
  mean / sd^2L
}

#' @rdname gamma-distr-conversion
gamma_mean <- function(shape, rate) {
  shape / rate
}

#' @rdname gamma-distr-conversion
gamma_sd <- function(shape, rate) {
  sqrt(shape) / rate
}
