deanomalize <- function(
  .data,
  .col,
  period = NULL,
  trend = NULL,
  cutoff = 0.05,
  quiet = FALSE,
  plot = FALSE,
  ...
) {

  col_nm <- coviData::select_colnames(.data, .col)

  anomalize::time_decompose(
    .data,
    target = col_nm,
    method = "stl",
    frequency = if (is.null(period)) "auto" else period,
    trend = if (is.null(trend)) "auto" else trend,
    message = !quiet,
    ...
  ) %>%
    anomalize::anomalize(
      target = "remainder",
      method = "gesd",
      alpha = cutoff,
      max_anoms = cutoff,
      verbose = FALSE
    ) %T>%
    {if (plot) anomalize::plot_anomalies(.) %>% show()} %>%
    anomalize::clean_anomalies()
}
