as_zoo <- function(.data, ..., .t) {
  col_nms <- select_colnames(.data, ...)
  t_nm <- select_colnames(.data, .t)

  assert_cols(.data, t_nm, n = 1L)

  if (vec_size(col_nms) == 1L) {
    zoo::zoo(x = .data[[col_nms]], order.by = .data[[t_nm]])
  } else {
    zoo::zoo(x = .data[col_nms], order.by = .data[[t_nm]])
  }
}
