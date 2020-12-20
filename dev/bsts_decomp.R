decomp_bsts <- function(.bsts, burn = 0.2) {

  likelihood = extract_likelihood_bsts(.bsts, burn = burn)

  dplyr::tibble(
    .t = timetk::tk_index(.bsts[["original.series"]]),
    observed = .bsts[["original.series"]] %>% as.vector(),
    season = extract_season_bsts(.bsts, burn = burn, likelihood = likelihood),
    trend = extract_trend_bsts(.bsts, burn = burn, likelihood = likelihood),
    remainder = extract_remainder_bsts(
      .bsts,
      burn = burn,
      likelihood = likelihood
    )
  )
}

extract_trend_bsts <- function(.bsts, burn = 0.2, likelihood = NULL) {

  dimensions <- dim(.bsts[["state.contributions"]])

  all_iterations <- seq_len(dimensions[[1]])
  all_observations <- seq_len(dimensions[[3]])

  if (rlang::is_null(likelihood)) {
    likelihood <- extract_likelihood_bsts(.bsts, burn = burn)
  }

  .bsts[["state.contributions"]] %>%
    extract(all_iterations, "trend", all_observations) %>%
    tibble::as_tibble() %>%
    dplyr::slice_tail(prop = 1 - burn) %>%
    dplyr::summarize(
      dplyr::across(.fns = ~ weighted.mean(.x, w = likelihood))
    ) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::pull(.data[["value"]])
}

extract_season_bsts <- function(.bsts, burn = 0.2, likelihood = NULL) {

  data <- .bsts[["original.series"]] %>% as.vector()

  if (rlang::is_null(likelihood)) {
    likelihood <- extract_likelihood_bsts(.bsts, burn = burn)
  }

  dimensions <- dim(.bsts[["state.contributions"]])

  all_iterations <- seq_len(dimensions[[1]])
  all_observations <- seq_len(dimensions[[3]])

  trend <- .bsts[["state.contributions"]] %>%
    extract(all_iterations, "trend", all_observations)

  .bsts[["one.step.prediction.errors"]] %>%
    t() %>%
    subtract(data) %>%
    multiply_by(-1) %>%
    t() %>%
    subtract(trend) %>%
    dplyr::as_tibble() %>%
    dplyr::slice_tail(prop = 1 - burn) %>%
    dplyr::summarize(
      dplyr::across(.fns = ~ weighted.mean(.x, w = likelihood))
    ) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::pull(.data[["value"]])
}

extract_remainder_bsts <- function(.bsts, burn = 0.2, likelihood = NULL) {

  data <- .bsts[["original.series"]] %>% as.vector()

  likelihood <- extract_likelihood_bsts(.bsts, burn = burn)

  .bsts[["one.step.prediction.errors"]] %>%
    dplyr::as_tibble() %>%
    dplyr::slice_tail(prop = 1 - burn) %>%
    dplyr::summarize(
      dplyr::across(.fns = ~ weighted.mean(.x, w = likelihood))
    ) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::pull(.data[["value"]])
}
