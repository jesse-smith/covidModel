new_rt <- function(
  x,
  nrow
) {
  tibble::new_tibble(x, nrow = nrow, class = "rt")
}

validate_rt <- function(x) {
  object_class <- attr(x, "class")
  rt_class <- c("rt", "tbl_df", "tbl", "data.frame")
  str_object_class <- stringr::str_flatten(object_class, ", ")
  str_rt_class <- stringr::str_flatten(rt_class, ", ")

  assertthat::assert_that(
    all(stringr::str_ends(str_object_class, pattern = str_rt_class)),
    msg = paste0(
      "This object's class is not 'rt'; ",
      "see below for a comparison of class attributes.\n\n",
      waldo::compare(object_class, rt_class)
    )
  )

  tibble::validate_tibble(x)

  vctrs::vec_assert(
    x,
    ptype = tibble::tibble(
      .t = as.Date(integer()),
      .pred = double(),
      .pred_lower = double(),
      .pred_upper = double()
    ) %>% new_rt(nrow = 0L)
  )

}


rt <- function(
  .t,
  .pred,
  .pred_lower,
  .pred_upper
) {

  tibble::tibble(
    .t = .t,
    .pred = .pred,
    .pred_lower = .pred_lower,
    .pred_upper = .pred_upper
  ) %>%
    validate_rt()
}

as_rt <- function(
  x,
  ...,
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal"),
  rownames = pkgconfig::get_config("tibble::rownames", NULL)
) {

  x %>%
    tibble::as_tibble(
      ...,
      .rows = .rows,
      .name_repair = .name_repair,
      rownames = rownames
    ) %>%
    new_rt(nrow = vec_size(.)) %>%
    validate_rt()
}
