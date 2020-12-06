estimate_rt <- function(
  .data,
  .col = NULL,
  .t = NULL,
  period = 7L,
  serial_interval_mean = 6,
  serial_interval_sd = 4.17,
  r0 = 3
) {
  
  EpiEstim::estimate_R(
    incid = prep_data_rt(
      .data,
      .col = .col,
      .t = .t
    ),
    config = prep_config_rt(
      .data,
      period = period,
      serial_interval_mean = serial_interval_mean,
      serial_interval_sd = serial_interval_sd
  ) %>% 
  tidy_rt()
  
}


prep_data_rt <- function(
  .data,
  .col = NULL,
  .t = NULL,
) {
  if (is.data.frame(.data)) {
    incid <- dplyr::select(
      dates = .t,
      I = .col
    )
  } else {
    incid <- .data %>%
      timetk::tk_tbl(rename_index = "dates") %>%
      dplyr::rename(I = .data[["x"]])
  }
}

prep_config_rt <- function(
  .data,
  period = 7L,
  serial_interval_mean = 6,
  serial_interval_sd = 4.5
) {

  t_start <- vec_seq_along(.data)
  
  EpiEstim::make_config(
    t_start = t_start,
    t_end = t_start + period - 1L,
    mean_si = serial_interval_mean,
    std_si = serial_interval_sd
  )
}

tidy_rt <- function(epiestim) {
  dplyr::as_tibble(epiestim[["R"]])
}
