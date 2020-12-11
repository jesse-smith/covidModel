estimate_rt <- function(
  .data,
  .col = "smoothed_cleaned",
  .t = ".t",
  period = 1L,
  serial_interval_mean = 6,
  serial_interval_sd = 4.17
) {

  EpiEstim::estimate_R(
    incid = prep_data_rt(
      .data,
      .col = .col,
      .t = .t
    ),
    method = "parametric_si",
    config = prep_config_rt(
      .data,
      period = period,
      serial_interval_mean = serial_interval_mean,
      serial_interval_sd = serial_interval_sd
    )
  ) %>%
    tidy_rt()
}

prep_data_rt <- function(
  .data,
  .col = NULL,
  .t = NULL
) {
  if (is.data.frame(.data)) {
    dplyr::select(
      .data,
      dates = .t,
      I = .col
    )
  } else {
    .data %>%
      timetk::tk_tbl(rename_index = "dates") %>%
      dplyr::rename(I = .data[["x"]])
  }
}

prep_config_rt <- function(
  .data,
  period = 7L,
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

tidy_rt <- function(rt) {
  rt[["R"]] %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      .t = vec_slice(rt[["dates"]], i = t_end)
    ) %>%
    dplyr::transmute(
      .t,
      .pred = `Median(R)`,
      .pred_lower = `Quantile.0.025(R)`,
      .pred_upper = `Quantile.0.975(R)`,
      .mean = `Mean(R)`,
      .cv = `Std(R)` / `Mean(R)`
    )
}

prep_linelist <- function(
  .data,
  .collection_date = collection_date,
  .report_date = report_date,
  start_date = "2020-03-12",
  trend = NULL,
  period = NULL,
  delay_period = 14L,
  pct_reported = 0.9,
  cutoff = 0.05,
  plot_anomalies = FALSE
) {

  .data %>%
    dplyr::select({{ .collection_date }}) %>%
    colnames() ->
  collect_nm

  .data %>%
    dplyr::select({{ .report_date }}) %>%
    colnames() ->
  report_nm

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
    {rlang::inform("")} %>%
    dplyr::count(.data[[collect_nm]]) %>%
    dplyr::mutate(n = log1p(.data[["n"]])) %>%
    deanomalize(
      .col = "n",
      trend = trend,
      period = period,
      cutoff = cutoff,
      plot = plot_anomalies
    ) %>%
    smooth_linelist("observed_cleaned", trend = trend, period = period) %>%
    dplyr::transmute(
      .t = .data[[collect_nm]],
      remainder_cleaned = .data[["observed_cleaned"]]  %>%
        subtract(.data[["trend"]] + .data[["season"]]) %>%
        expm1(),
      observed = .data[["observed"]] %>% expm1() %>% pmax(0),
      observed_cleaned = .data[["observed_cleaned"]] %>% expm1() %>% pmax(0),
      smoothed_cleaned = .data[["smoothed"]] %>% expm1() %>% pmax(0)
    ) %>%
    dplyr::relocate(
      .data[["remainder_cleaned"]],
      .after = .data[["smoothed_cleaned"]]
    )
}

filter_reg <- function(x, degree = 2L) {

  size <- vec_size(x)

  i <- vec_seq_along(x)

  degree <- min(size - 1L, degree)

  if (size < 7L | degree <= 0L) {
    weighted.mean(x, w = i, na.rm = TRUE)
  } else {
    lm(
      x ~ stats::poly(i, degree = degree),
      weights = i,
      na.action = na.exclude
    ) %>%
      predict() %>%
      extract2(size)
  }
}

smooth_linelist <- function(.data, .col, trend = NULL, period = NULL) {

  if (is.null(period)) {
    period <- "auto"
  }

  if (is.null(trend)) {
    trend <- "auto"
  }

  .t <- timetk::tk_get_timeseries_variables(.data)[[1]]

  .period <- timetk::tk_get_frequency(
    .data[[.t]],
    period = period,
    message = FALSE
  )

  .trend <- timetk::tk_get_trend(
    .data[[.t]],
    period = trend,
    message = FALSE
  )

  filterrr <- timetk::slidify(
    filter_reg,
    .period = .trend,
    .align = "right",
    .partial = TRUE
  )

  .data %>%
    decompose_time_stl(
      .col = .col,
      trend = .trend,
      period = .period
    ) %>%
    dplyr::pull(.data[["trend"]]) ->
  smth

  dplyr::mutate(.data, smoothed = smth)

}

gamma_shape <- function(mean, sd) {
  (mean / sd)^2L
}

gamma_rate <- function(mean, sd) {
  mean / sd^2L
}

gamma_mean <- function(shape, rate) {
  shape / rate
}

gamma_sd <- function(shape, rate) {
  sqrt(shape) / rate
}

filter_rt <- function(.data, trend = NULL, degree = 2L) {

  if (is.null(trend)) {
    trend <- "auto"
  }

  .trend <- timetk::tk_get_trend(
    .data[[".t"]],
    period = trend,
    message = FALSE
  )

  filterrr <- timetk::slidify(
    ~ filter_reg(.x, degree = degree),
    .period = .trend,
    .align = "right",
    .partial = TRUE
  )

  .data %>%
    dplyr::mutate(
      .cv = filterrr(.data[[".cv"]]),
      .mean = filterrr(.data[[".mean"]]),
      .sd = .data[[".cv"]] * .data[[".mean"]],
      .shape = gamma_shape(.data[[".mean"]], .data[[".sd"]]),
      .rate = gamma_rate(.data[[".mean"]], .data[[".sd"]])
    ) %>%
    dplyr::transmute(
      .t = .data[[".t"]],
      .pred = qgamma(0.5, shape = .data[[".shape"]], rate = .data[[".rate"]]),
      .pred_lower = qgamma(0.025, shape = .shape, rate = .rate),
      .pred_upper = qgamma(0.975, shape = .shape, rate = .rate),
      .mean = .data[[".mean"]],
      .cv = .data[[".cv"]]
    )
}
