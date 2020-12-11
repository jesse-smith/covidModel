resample_rt <- function(x, period) {

  i <- vec_seq_along(x)

  size <- vec_size(x)
  half_p <- as.integer(ceiling(period / 2L) - 1L)

  ref <- smooth_x(x, period)


  purrr::map(
    period:size,
    ~ .fn(x[1L:.x], period) %>%
      subtract(ref[1L:.x]) %>%
      multiply_by(-1L) %>%
      rev() %>%
      extract(1L:half_p) %>%
      rev() %>%
      {dplyr::if_else(dplyr::near(., 0), 0, .)}
  ) %>%
    purrr::transpose() %>%
    set_names(paste0("h", stringr::str_pad(1L:vec_size(.), width = 2, pad = 0))) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(.fns = ~ unlist(.x)))
}

smooth_x <- function(x, period) {
  loess(
    x ~ vec_seq_along(x),
    span = period / vec_size(x),
    family = "symmetric",
    degree = 1,
    surface = "direct",
    statistics = "none"
  )$fitted
}

