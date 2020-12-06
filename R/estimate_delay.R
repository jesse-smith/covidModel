# Weight average by number of people, recency, or none?
estimate_delay <- function(
  .data,
  .collection_date = collection_date,
  .report_date = report_date,
  pct = 0.9,
  period = 14L,
  today = Sys.Date(),
  rtn = c("with_last_complete", "incomplete_only", "all"),
  min_dt = as.Date("2020-04-12")
) {

  rtn <- rlang::arg_match(rtn)[[1]]

  # Releases memory if `.data` has additional columns
  data <- dplyr::select(.data, {{ .collection_date }}, {{ .report_date }})
  remove(.data)

  assertthat::assert_that(
    NCOL(data) == 2,
    msg = paste0(
      "The selections for `.collection_date` and `.report_date` ",
      "much match exactly one column each"
    )
  )

  # Column names are easier to reference by and make for more informative errors
   collect_nm <- data %>%
    dplyr::select({{ .collection_date }}) %>%
    colnames()

  report_nm <- data %>%
    dplyr::select({{ .report_date }}) %>%
    colnames()

  # Each selection should match one column
  purrr::when(
    vec_size(collect_nm),
    . > 1 ~ rlang::abort(
      paste0(
        rlang::enexpr(.collection_date) %>% rlang::expr_label(),
        "\n\n",
        "must select one column, but it matches multiple:\n",
        rlang::format_error_bullets(collect_nm)
      )
    ),
    . < 1 ~ rlang::abort(
      paste0(
        rlang::enexpr(.collection_date) %>% rlang::expr_label(),
        "\n\n",
        "must select one column, but it matches none."
      )
    )
  )

  purrr::when(
    vec_size(report_nm),
    . > 1 ~ rlang::abort(
      paste0(
        rlang::enexpr(.report_date) %>% rlang::expr_label(),
        "\n\n",
        "must select one column, but it matches multiple:\n",
        rlang::format_error_bullets(report_nm)
      )
    ),
    . < 1 ~ rlang::abort(
      paste0(
        rlang::enexpr(.report_date) %>% rlang::expr_label(),
        "\n\n",
        "must select one column, but it matches none."
      )
    )
  )

  # Variables must be dates
  assertthat::assert_that(
    lubridate::is.Date(data[[collect_nm]]),
    msg = paste0(collect_nm, " must be of type `Date`")
  )

  assertthat::assert_that(
    lubridate::is.Date(data[[report_nm]]),
    msg = paste0(report_nm, " must be of type `Date`")
  )
  
  # Prefer robust mean, but use non_robust if period isn't long enough 
  if (period >= 14L) {
    wt_mean <- function(x, w) {
      MASS::rlm(
        x ~ 1L,
        weights = w,
        na.action = na.exclude,
        wt.method = "case",
        maxit = 1e4
      ) %>% coef()
    }
  } else {
    rlang::warn("`period` is less than 14 days; using non-robust average")
    wt_mean <- function(x, w) {
      weighted.mean(
        x,
        w = w,
        na.rm = TRUE
      )
    }
  }
  # Rolling weighted mean
  wt_mean_rolling <- timetk::slidify(
    ~ wt_mean(.x, w = .y),
    .period = period,
    .align = "right",
    .partial = FALSE,
    .unlist = TRUE
  )

  removing_min_dt <- paste0("Removing collection dates earlier than ", min_dt)

  filtering_illogical_or_missing <- paste0(
    "Filtering logically inconsistent or missing observations"
  )

  data %T>%
    # Inform user of filtered observations
    {rlang::inform(filtering_illogical_or_missing)} %>%
    tidylog::filter(
      dplyr::between(.data[[collect_nm]], as.Date("2020-03-05"), Sys.Date()),
      dplyr::between(.data[[report_nm]], as.Date("2020-03-05"), Sys.Date()),
      .data[[collect_nm]] <= .data[[report_nm]],
      !is.na(.data[[collect_nm]]),
      !is.na(.data[[report_nm]])
    ) %T>%
    {rlang::inform(removing_min_dt)} %>%
    tidylog::filter(.data[[collect_nm]] >= min_dt) %>%
    dplyr::mutate(
      delay = as.integer(.data[[report_nm]] - .data[[collect_nm]])
	  ) %>%
    dplyr::group_by(.data[[collect_nm]]) %>%
    dplyr::summarize(
      delay = quantile(delay, prob = pct, type = 8),
      n = dplyr::n()
    ) %>%
    fill_dates(!!rlang::sym(collect_nm), end = today) %>%
    dplyr::transmute(
      .data[[collect_nm]],
      prior_delay = wt_mean_rolling(.data[["delay"]], .data[["n"]]),
      t_from_today = as.integer(today - .data[[collect_nm]]),
      incomplete = (round(prior_delay) >= t_from_today) %>%
        tidyr::replace_na(FALSE) %>%
        dplyr::cumany()
    ) %>%
    dplyr::select(-t_from_today) %>%
    purrr::when(
      rtn == "with_last_complete" ~ dplyr::mutate(
          .,
          f = dplyr::cumany(dplyr::lead(.data[["incomplete"]]))
        ) %>%
        dplyr::filter(f) %>%
        dplyr::select(-f),
      rtn == "incomplete_only" ~ dplyr::filter(., .data[["incomplete"]]),
      rtn == "all" ~ .
    ) %>%
    dplyr::mutate(
      delay = today - .data[["collection_date"]],
      .before = "incomplete"
    )
}

