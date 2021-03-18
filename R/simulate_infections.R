simulate_infections <- function(rt, vac_init, vac_per_day = 3000, vac_init_pct = 0.25, vac_add_pct = vac_init_pct, h = 1L, population = 1e6, detection = 1/3) {

  # Adjust incidence for detection rate
  rt <- dplyr::mutate(rt, .incid = .incid / detection)

  # Get parameters up to latest time point
  .t <- rt[[".t"]]
  incid <- rt[[".incid"]]
  cumulative_incid <- incid %>% tidyr::replace_na(0) %>% cumsum()
  active <- estimate_activity(rt)

  # Get starting values for simulation
  input_size <- vec_size(rt)
  next_incid_diff <- incid[[input_size]] - incid[[input_size - 1L]]
  next_incid <- incid[[input_size]] + next_incid_diff

  # Parameterize rt and serial interval
  current_rt <- rt[[".mean"]][[input_size]]
  serial_interval_params <- attr(rt, "serial_interval")

  # Get initial susceptible percentage
  initial_susceptible <- population %>%
    subtract(cumulative_incid[[vec_size(cumulative_incid)]]) %>%
    subtract(vac_init * vac_init_pct)
  initial_susceptible_pct <- initial_susceptible / population

  for (t in seq_len(h)) {

    # Get current variables
    current_time <- .t[[input_size - 1L + t]]
    current_cumulative_incid <- cumulative_incid[[input_size - 1L + t]]
    current_active <- active[[input_size - 1L + t]]
    if (t == 1) {
      current_susceptible <- initial_susceptible
    } else {
      current_susceptible <- population - current_cumulative_incid - vac_per_day * vac_add_pct
    }

    current_susceptible_pct <- current_susceptible / population

    susceptible_adjustment <- current_susceptible_pct / initial_susceptible_pct

    new_incid <- current_rt * current_active * susceptible_adjustment

    # The first simulated incidence needs to be aligned with the previously
    # estimated one
    if (t == 1) {
      incid_correction <- next_incid - new_incid
    }

    new_incid <- new_incid + incid_correction

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
