estimate_rt <- function(
  .data,
  .col = "smoothed_cleaned",
  .t = ".t",
  period = 7L,
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
    dplyr::select(
      .t,
      .pred = `Median(R)`,
      .pred_lower = `Quantile.0.025(R)`,
      .pred_upper = `Quantile.0.975(R)`,
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
      observed = .data[["observed"]] %>% expm1() %>% pmax(0),
      observed_cleaned = .data[["observed_cleaned"]] %>% expm1() %>% pmax(0),
      smoothed_cleaned = .data[["smoothed"]] %>% expm1() %>% pmax(0)
    )
}

smooth_linelist <- function(.data, .col, trend = NULL, period = NULL) {
  .data %>%
    decompose_time_stl(
      .col = .col,
      trend = if (is.null(trend)) "auto" else trend,
      period = if (is.null(period)) "auto" else period
    ) %>%
    dplyr::transmute(
      smoothed = smoots::tsmooth(
        .data[["trend"]] + .data[["remainder"]],
        bStart = 0.1
      )$ye
    ) %>%
    dplyr::pull(.data[["smoothed"]]) ->
  smoothed

  dplyr::mutate(.data, smoothed = smoothed)

}
