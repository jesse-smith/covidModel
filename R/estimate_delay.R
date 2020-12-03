estimate_delay <- function(
  .data,
  .collection_date = collection_date,
  .report_date,
  prob = 0.9
) {

  .data %>%
    dplyr::select(
      .collection_date,
      .report_date
    ) %>%
    tidylog::filter(
      .data[[.collection_date]] <= .data[[.report_date]]
	) %>%
    dplyr::mutate(
      delay = as.numeric(
        .report_date - .collection_date
      )
	) %>%
	tidylog::count(.collection_date, delay) %>%
    dplyr::arrange(.collection_date, delay) ->
  data_long
  
  remove(.data)
  
  data_long %>%
    tidyr::pivot_wider(
      names_from = delay,
      names_prefix = "delay_",
      values_from = n,
      values_fill = 0
    ) ->
  data_wide
  
  data_wide %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("delay_"),
        ~ timetk::slidify_vec(
          .x,
          ~ sum(.x, na.rm = TRUE),
          .period = 7,
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
    dplyr::arrange(.collection_date, delay) %>%
    dplyr::group_by(.collection_date) %>%
    dplyr::summarize(
      q = n %>%
        tidyr::na_replace(0) %>%
        cumsum() %>%
        divide_by(sum(n, na.rm = TRUE)) %>%
        is_weakly_greater_than(prob) %>%
        vec_slice(x = delay) %>%
        extract2(1),
	) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      from_today = q %>% seq_along() %>% rev()
	) %>%
    dplyr::filter(q > from_today - 1)

}