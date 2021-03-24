library(ggplot2)

purrr::map_dfr(
  c(0.85, 0.9, 0.95, 1, 1.05, 1.1, 1.2),
  ~ dplyr::mutate(sirv(active_cases = 150, end = "2021-06-01", r0 = .x), r0 = .x) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["i"]] / 3, group = .data[["r0"]]) +
  coord_cartesian(ylim = c(0, 1000))
