
estimate_delay <- function(
  .data,
  .collection_date = collection_date,
  .report_date = report_date,
  pct = 0.9,
  period = 14L,
  today = Sys.Date(),
  rtn = c("last_complete", "incomplete_only", "all"),
  min_dt = as.Date("2020-04-12"),
  quiet = FALSE
) {

  rtn <- rlang::arg_match(rtn)[[1]]

  collect_expr <- rlang::enquo(.collection_date)
  report_expr <- rlang::enquo(.report_date)

  collect_nm <- coviData::select_colnames(.data, !!collect_expr)
  report_nm <- coviData::select_colnames(.data, !!report_expr)

  coviData::assert_cols(
    .data,
    collect_nm,
    ptype = lubridate::Date(),
    n = 1L
  )

  coviData::assert_cols(
    .data,
    report_nm,
    ptype = lubridate::Date(),
    n = 1L
  )


  # Releases memory if `.data` has additional columns
  data <- dplyr::select(.data, collect_nm, report_nm)
  remove(.data)

  collect_idx <- data[[collect_nm]] %>% unique() %>% sort()

  period <- timetk::tk_get_frequency(
    collect_idx,
    period = period,
    message = FALSE
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
    if (!quiet) {
      rlang::inform("`period` is less than 14 days; using non-robust average")
    }
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

  removing_min_dt <- paste0(
    "Removing collection dates earlier than ", min_dt
  )

  filtering_illogical_or_missing <- paste0(
    "Removing logically inconsistent or missing observations"
  )

  body <- rlang::expr({
    data %T>%
      # Inform user of filtered observations
      {if (!quiet) rlang::inform(filtering_illogical_or_missing)} %>%
      tidylog::filter(
        dplyr::between(.data[[collect_nm]], as.Date("2020-03-05"), Sys.Date()),
        dplyr::between(.data[[report_nm]], as.Date("2020-03-05"), Sys.Date()),
        .data[[collect_nm]] <= .data[[report_nm]],
        !is.na(.data[[collect_nm]]),
        !is.na(.data[[report_nm]])
      ) %T>%
      {if (!quiet) rlang::inform(removing_min_dt)} %>%
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
        rtn == "last_complete" ~ dplyr::mutate(
            .,
            f = dplyr::cumany(dplyr::lead(.data[["incomplete"]]))
          ) %>%
          dplyr::filter(.data[["f"]]) %>%
          dplyr::select(-.data[["f"]]) %>%
          dplyr::filter(
            .data[[collect_nm]] == min(.data[[collect_nm]], na.rm = TRUE)
          ),
        rtn == "incomplete" ~ dplyr::filter(., .data[["incomplete"]]),
        rtn == "all" ~ .
      ) %>%
      dplyr::mutate(
        delay = today - .data[[collect_nm]],
        .before = "incomplete"
      )
  })

  if (quiet) {
    suppressMessages(
      suppressWarnings(
        eval(body)
      )
    )
  } else {
    eval(body)
  }
}

