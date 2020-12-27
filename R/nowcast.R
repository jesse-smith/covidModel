#' Nowcast Cases by Specimen Collection Date
#'
#' `nowcast_cases()` predicts the number of cases expected through the current
#' date given a linelist of cases with collection and report dates. It uses
#' \code{\link[covidModel:estimate_delay]{estimate_delay()}} to determine the
#' last complete collection date and forecasts from that point. Cases are
#' predicted by a weighted ensemble of two log-linear structural time series
#' models with semilocal and robust (Student-t) trends.
#'
#' @param .data A linelist of incidence cases as a data frame
#'
#' @param .collection_date A `Date` column containing the dates that cases
#'   were tested
#'
#' @param .report_date A `Date` column containing the dates that cases were
#'   reported
#'
#' @param today The date to consider "today"
#'
#' @param iterations The number of MCMC iterations for each model in the
#'   ensemble; the total number of iterations will be twice this amount.
#'
#' @param burn The percentage of initial iterations to discard for each model
#'
#' @export
nowcast_cases <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  today = Sys.Date(),
  iterations = ceiling(1e4 / (1 - burn)),
  burn = 0.2
) {

  collect_nm <- select_colnames(.data, .collection_date)
  report_nm <- select_colnames(.data, .report_date)

  count_data <- .data %>%
    clean_linelist(.collection_date = collect_nm, .report_date = report_nm) %>%
    dplyr::count(.data[[collect_nm]], name = "cases") %>%
    {suppressMessages(timetk::pad_by_time(., .data[[collect_nm]]))}

  remove(.data)

  new_dates <- seq(max(count_data[[collect_nm]]) + 1L, today, by = 1L)

  predictions <- count_data %>%
    dplyr::mutate(cases = log1p(.data[["cases"]])) %>%
    dplyr::summarize(
      robust = model_incidence(
        .,
        "cases",
        collect_nm,
        iterations = iterations,
        method = "robust"
      ) %>% list()
    ) %>%
    dplyr::mutate(
      error_robust = .data[["robust"]] %>%
        extract2(1L) %>%
        extract_error_bsts(burn = burn)
    ) %>%
    dplyr::summarize(
      predict_robust = robust[[1L]] %>%
        predict(h = vec_size(new_dates), burn = ceiling(burn * iterations)) %>%
        extract2("median") %>%
        expm1()
    ) %>%
    dplyr::pull("predict_robust") %>%
    round() %>%
    as.integer()

  predicted_count_data <- dplyr::tibble(
    .t = new_dates,
    cases = predictions,
    predicted = TRUE
  ) %>%
    dplyr::rename( {{ collect_nm }} := .data[[".t"]])

  count_data %>%
    dplyr::mutate(predicted = FALSE) %>%
    dplyr::bind_rows(predicted_count_data)
}

#' Nowcast Testing Numbers by Collection Date
#'
#' `nowcast_tests()` predicts the expected number of tests per day through the
#' current date given a linelist of cases with collection and report dates. It
#' uses \code{\link[covidModel:estimate_delay]{estimate_delay()}} to determine
#' the last complete collection date and forecasts from that point. Tests are
#' predicted by a weighted ensemble of two structural time series models with
#' semilocal and robust (Student-t) trends and a square-root link.
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
