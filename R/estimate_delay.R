# Weight average by number of people, recency, or none?
estimate_delay <- function(
  .data,
  .collection_date = collection_date,
  .report_date = report_date,
  prob = 0.9,
  period = 7L
) {

  # Release memory if `.data` has additional columns
  data <- dplyr::select(
    .data,
    {{ .collection_date }},
    {{ .report_date }}
  )
  remove(.data)
  
  # Check that `data` is 2 columns
  assert_that(
    NCOL(data) == 2,
    msg = paste0(
      "The selections for ",
      "`.collection_date` and ",
      "`.report_date` much match ",
      "exactly one column each"
    )
  )
  
  # Column names are easier to reference by and make for more informative errors
   c_nm <- data %>% 
    dplyr::select({{ .collection_date }}) %>%
    colnames()
  r_nm <- data %>%
    dplyr::select({{ .report_date }}) %>%
    colnames()
    
  # Each selection should match one column
  purrr::when(
    vec_size(c_nm),
    . > 1 ~ rlang::abort(
      paste0(
        rlang::enexpr(.collection_date) %>%
        rlang::expr_label(),
        "\n\n",
        "must select one column, ",
        "but it matches multiple: ",
        rlang::format_error_bullets(c_nm)
      )
    ),
    . < 1 ~ rlang::abort(
      paste0(
        rlang::enexpr(.collection_date) %>%
        rlang::expr_label(),
        "\n\n",
        "must select one column ",
        "but it matches none."
      )
    )
  )
  
  purrr::when(
    vec_size(r_nm),
    . > 1 ~ rlang::abort(
      paste0(
        rlang::enexpr(.report_date) %>%
        rlang::expr_label(),
        "\n\n",
        "must select one column, ",
        "but it matches multiple: ",
        rlang::format_error_bullets(r_nm)
      )
    ),
    . < 1 ~ rlang::abort(
      paste0(
        rlang::enexpr(.report_date) %>%
        rlang::expr_label(),
        "\n\n",
        "must select one column ",
        "but it matches none."
      )
    )
  )
  
  # reference using symbols for better error messages
  c_sym <- rlang::sym(c_nm)
  r_sym <- rlang::sym(r_nm)
  
  rob_mean_sliding <- timetk::slidify(
    ~ MASS::rlm(. ~ 1L, method = "MM") %>%
      coef(),
    .period = period,
    .align = "right",
    .unlist = TRUE
  )
  
  data %>%
  # Function is designed to work with dates
    assertr::assert(lubridate::is.Date, dplyr::everything()) %>%
    # Collection date can't be after report date; print filter message to notify user of removals
    tidylog::filter(
      !!c_sym <= !!r_sym
    ) %>%
    # Calculate delay
    dplyr::mutate(
      delay = as.numeric(
        !!r_sym - !!c_sym
      )
	) %>%
	# Operate within each date
	dplyr::group_by(!!c_sym) %>%
	# Calculate quantile
	dplyr::summarize(
	  threshold = delay %>%
	    stats::quantile(
	      prob = prob,
	      type = 8
	    ) %>% 
	    as.integer()
	) %>%
	dplyr::ungroup() %>%
	dplyr::arrange(!!c_sym) %>%
	dplyr::mutate(
	  threshold = threshold %>%
	    rob_mean_sliding()
	  delay = .data[["threshold"]] %>%
	    vec_seq_along() %>%
	    vec_order("desc")
	) %>%
	dplyr::filter(
	  .data[["threshold"]] > .data[["delay"]] - 1L
	) %>%
	dplyr::filter(
    .data[["delay"]] == max(.data[["delay"]], na.rm = TRUE)
  ) %>%
  dplyr::select(!!c_sym, .data[["delay"]])
# Weighted by person ###########################
	# Count
	tidylog::count(!!c_sym, delay, sort = TRUE) %>%
    tidyr::pivot_wider(
      names_from = delay,
      names_prefix = "delay_",
      values_from = n,
      values_fill = 0
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("delay_"),
        ~ timetk::slidify_vec(
          .x,
          ~ sum(.x, na.rm = TRUE),
          .period = period,
          .align  = "right",
          .partial = TRUE
		)
	  )
	) %>%
    tidyr::pivot_longer(
      dplyr::starts_with("delay_"),
      names_to = "delay",
      names_prefix = "delay_",
      names_transform = list(delay = as.integer),
      values_to = "n",
      values_transform = list(n = as.integer)
	) %>%
    dplyr::arrange(!!c_sym, delay) %>%
    dplyr::group_by(!!c_sym) %>%
    dplyr::summarize(
      threshold = .data[["n"]] %>%
        tidyr::na_replace(0) %>%
        cumsum() %>%
        divide_by(sum(.data[["n"]]), na.rm = TRUE)) %>%
        is_weakly_greater_than(prob) %>%
        vec_slice(x = .data[["delay"]]) %>%
        extract2(1),
	) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      delay = .data[["threshold"]] %>% 
      vec_seq_along() %>% 
      vec_order("desc")
	) %>%
    dplyr::filter(.data[["threshold"]] > .data[["delay"]] - 1L) %>%
  dplyr::filter(
    .data[["delay"]] == max(.data[["delay"]], na.rm = TRUE)
  ) %>%
  dplyr::select(!!c_sym, .data[["delay"]])

}