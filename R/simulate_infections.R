simulate_infections <- function(rt, h = 1L, population = 1e6, detection = 1) {

  rt <- dplyr::mutate(rt, .incid = .data[[".incid"]] / {{ detection }})

  .t <- rt[[".t"]]

  incid <- rt[[".incid"]]

  cumulative_incid <- incid %>% tidyr::replace_na(0) %>% cumsum()

  active <- estimate_activity(rt)

  input_size <- vec_size(rt)

  next_incid_diff <- incid[[input_size]] - incid[[input_size - 1L]]

  next_incid <- incid[[input_size]] + next_incid_diff

  current_rt <- rt[[".mean"]][[input_size]]

  serial_interval_params <- attr(rt, "serial_interval")

  initial_susceptible_pct <- population %>%
    subtract(cumulative_incid[[vec_size(cumulative_incid)]]) %>%
    divide_by(population)

  for (t in seq_len(h)) {

    # Get current variables
    current_time <- .t[[input_size - 1L + t]]
    current_cumulative_incid <- cumulative_incid[[input_size - 1L + t]]
    current_active <- active[[input_size - 1L + t]]
    current_susceptible <- population - current_cumulative_incid

    current_susceptible_pct <- current_susceptible / population

    susceptible_adjustment <- current_susceptible_pct / initial_susceptible_pct

    new_incid <- current_rt * current_active * susceptible_adjustment

    # if (t == 1) {
    #   incid_correction <- next_incid - new_incid
    # }

    # new_incid <- max(new_incid + incid_correction, 0)

    # Update totals
    .t <- append(.t, current_time + 1)

    incid <- append(incid, new_incid)

    cumulative_incid <- append(
      cumulative_incid,
      current_cumulative_incid + new_incid
    )

    active <- estimate_activity(
      dplyr::tibble(.t = .t, .incid = incid),
      incid = ".incid",
      .t = ".t",
      serial_interval_mean = serial_interval_params[[1L]],
      serial_interval_sd = serial_interval_params[[2L]]
    )
  }

  incid * detection
}
