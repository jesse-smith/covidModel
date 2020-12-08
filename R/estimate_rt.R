estimate_rt <- function(
  .data,
  .col = NULL,
  .t = NULL,
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
    tidy_rt() %>%
    as_rt()
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
  epiestim[["R"]] %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      .t = vec_slice(epiestim[["dates"]], i = t_end)
    ) %>%
    dplyr::select(
      .t,
      .pred = `Median(R)`,
      .pred_lower = `Quantile.0.025(R)`,
      .pred_upper = `Quantile.0.975(R)`,
    )
}
