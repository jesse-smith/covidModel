#' Estimate Infectious Activity from Incidence and Serial Interval
#'
#' @param .data Daily incidence data in data frame or `covidmodel_rt` format
#'
#' @param incid Character. The column containing incidence data
#'
#' @param .t Character. The time column.
#'
#' @param serial_interval_mean Numeric. The average of the serial interval.
#'
#' @param serial_interval_sd Numeric. The standard deviation of the serial
#'   interval.
#'
#' @export
estimate_activity <- function(
  .data,
  incid,
  .t,
  serial_interval_mean,
  serial_interval_sd
) {
  UseMethod("estimate_activity")
}

#' @rdname estimate_activity
#'
#' @export
estimate_activity.data.frame <- function(
  .data,
  incid = "trend",
  .t = "collection_date",
  serial_interval_mean = 6,
  serial_interval_sd = 4.17
) {
  filter_by_serial_interval(
    .data,
    incid = incid,
    .t = .t,
    serial_interval_mean = serial_interval_mean,
    serial_interval_sd = serial_interval_sd
  )
}

#' @rdname estimate_activity
#'
#' @export
estimate_activity.covidmodel_rt <- function(
  .data,
  incid = ".incid",
  .t = ".t",
  serial_interval_mean = attr(.data, "serial_interval")[[1L]],
  serial_interval_sd = attr(.data, "serial_interval")[[2L]]
) {

  # Extract serial interval parameters from object
  serial_interval_params <- attr(.data, "serial_interval")

  # Filter
  filter_by_serial_interval(
    .data,
    incid = ".incid",
    .t = ".t",
    serial_interval_mean = serial_interval_params[[1L]],
    serial_interval_sd = serial_interval_params[[2L]]
  )
}

filter_by_serial_interval <- function(
  .data,
  incid = "trend",
  .t = "collection_date",
  serial_interval_mean = 6,
  serial_interval_sd = 4.17
) {

  incid_quo <- rlang::enquo(incid)
  .t_quo <- rlang::enquo(.t)

  incid_nm <- coviData::select_colnames(.data, !!incid_quo)
  .t_nm <- coviData::select_colnames(.data, !!.t_quo)

  serial_interval <- discretize_serial_interval(
    mean = serial_interval_mean,
    sd = serial_interval_sd
  )

  EpiEstim::overall_infectivity(
    incid = prep_data_rt(.data, .incid = incid_nm, .t = .t_nm),
    si_distr = serial_interval
  )
}

discretize_serial_interval <- function(mean = 6, sd = 4.17, cutoff = 0.99) {

  # Error handling

  cutoff_error_msg <- paste0(
    "`cutoff` must be a decimal between 0 and 1 ",
    "or a whole number greater than 1"
  )

  cutoff_is_scalar_numeric <- any(
    vec_is(cutoff, integer(), size = 1L),
    vec_is(cutoff, double(), size = 1L)
  )

  if (!cutoff_is_scalar_numeric) {
    rlang::abort(cutoff_error_msg)
  }

  cutoff_is_count <- dplyr::near(cutoff, round(cutoff))

  cutoff_is_probability <- cutoff < 1 && !cutoff_is_count

  if (!(cutoff_is_count || cutoff_is_probability)) {
    rlang::abort(cutoff_error_msg)
  }

  # Get max time period for distribution from `cutoff`

  if (cutoff_is_probability) {
    shape <- gamma_shape(mean = mean, sd = sd)
    rate <- gamma_rate(mean = mean, sd = sd)

    k_max <- stats::qgamma(cutoff, shape = shape, rate = rate) %>% ceiling()
  } else {
    k_max <- ceiling(cutoff)
  }

  # Discretize serial interval

  EpiEstim::discr_si(
    k = seq(0L, as.integer(k_max), by = 1L),
    mu = mean,
    sigma = sd
  )
}

tidy_infectious_activity <- function() {

}
