new_covidmodel_rt <- function(x, serial_interval, nrow) {
  tibble::new_tibble(
    x,
    serial_interval = serial_interval,
    nrow = nrow,
    class = "covidmodel_rt"
  )
}

validate_covidmodel_rt <- function(x) {

  si <- attr(x, "serial_interval", exact = TRUE)

  # `serial_interval` must exist
  if (is.null(si)) {
    rlang::abort(
      "`x` must have a `serial_interval` attribute",
      class = "covidmodel_rt_no_serial_interval"
    )
  }

  # `serial_interval` must must be a `double` vector of size 2
  vec_assert(si, ptype = double(), size = 2L)

  # `x` must meet all the usual requires of a tibble
  tibble::validate_tibble(x)
}

covidmodel_rt <- function(x, serial_interval) {
  x %>%
    new_covidmodel_rt(serial_interval = serial_interval, nrow = vec_size(.)) %>%
    validate_covidmodel_rt()
}
