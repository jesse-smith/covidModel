model_hospital <- function(
  census,
  dates,
  niter = ceiling(1e4 / 0.8)
) {
  z <- census %>% timetk::log_interval_vec(
    limit_lower = 0,
    limit_upper = 1400,
    offset = 1
  ) %>%
    zoo::zoo()

  zoo::index(z) <- dates

  bsts::AddLocalLinearTrend(y = z) %>%
    bsts::bsts(formula = z, niter = niter) ->
  model

  attr(model, which = "trans") <- log1p
  attr(model, which = "inv_trans") <- expm1

  model
}

model_incidence <- function(
  .data,
  .col,
  .t,
  iterations = ceiling(1e4 / 0.8),
  method = c("semilocal", "local", "robust")
) {

  method <- rlang::arg_match(method)[[1L]]

  col_nm <- select_colnames(.data, .col)
  t_nm <- select_colnames(.data, .t)

  assert_cols(.data, col_nm, n = 1L)
  assert_cols(.data, t_nm, ptype = lubridate::Date(), n = 1L)

  .data %>%
    as_zoo(col_nm, .t = t_nm) %>%
    list() %>%
    set_names(".data") %>%
    bsts_trend(method = method) %>%
    bsts_season(method = "regression", period = "7 days", season = "1 day") %>%
    bsts_fit(iterations = iterations)
}

#' Nowcast
nowcast_tests <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  today = Sys.Date(),
  iterations = ceiling(1e4 / 0.8),
  burn = 0.2
) {

  collect_nm <- select_colnames(.data, .collection_date)
  report_nm <- select_colnames(.data, .report_date)

  count_data <- .data %>%
    clean_linelist(.collection_date = collect_nm, .report_date = report_nm) %>%
    dplyr::count(.data[[collect_nm]], name = "tests") %>%
    {suppressMessages(timetk::pad_by_time(., .data[[collect_nm]]))}

  new_dates <- seq(max(count_data[[collect_nm]]) + 1L, today, by = 1L)

  predictions <- count_data %>%
    dplyr::mutate(tests = sqrt(.data[["tests"]])) %>%
    dplyr::summarize(
      semilocal = model_incidence(
        .,
        "tests",
        collect_nm,
        iterations = iterations,
        method = "semilocal"
      ) %>% list(),
      local = model_incidence(
        .,
        "tests",
        collect_nm,
        iterations = iterations,
        method = "local"
      ) %>% list(),
      robust = model_incidence(
        .,
        "tests",
        collect_nm,
        iterations = iterations,
        method = "robust"
      ) %>% list()
    ) %>%
    dplyr::summarize(
      predict_semilocal = semilocal[[1L]] %>%
        predict(h = vec_size(new_dates), burn = ceiling(burn * iterations)) %>%
        extract2("median") %>%
        raise_to_power(2),
      predict_local = local[[1L]] %>%
        predict(h = vec_size(new_dates), burn = ceiling(burn * iterations)) %>%
        extract2("median") %>%
        raise_to_power(2),
      predict_robust = robust[[1L]] %>%
        predict(h = vec_size(new_dates), burn = ceiling(burn * iterations)) %>%
        extract2("median") %>%
        raise_to_power(2)
    ) %>%
    rowMeans() %>%
    round() %>%
    as.integer()

  predicted_count_data <- dplyr::tibble(
    .t = new_dates,
    tests = predictions,
    predicted = TRUE
  ) %>%
    dplyr::rename( {{ collect_nm }} := .data[[".t"]])

  count_data %>%
    dplyr::mutate(predicted = FALSE) %>%
    dplyr::bind_rows(predicted_count_data)
}

