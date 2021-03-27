library(ggplot2)
library(magrittr)
library(covidModel)


p <- dplyr::mutate(sirv(active_cases = 500, end = "2021-06-01", rt = 1.05, pct_var = 0.85, pct_var_uk = 0.1 / 0.85), `Rb` = 1.00)

purrr::map_dfr(
  c(seq(0.7, 0.95, by = 0.05), seq(1.05, 1.3, by = 0.05)),
  ~ dplyr::mutate(sirv(active_cases = 500/.x, end = "2021-06-01", r0 = .x), `Rb` = .x) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["i_obs"]], group = .data[["Rb"]]) +
  geom_line(data = p, aes(x = time, y = i_obs), color = "goldenrod3", size = 1.5) +
  annotate(
    "text",
    x = as.Date('2021-06-01'),
    y = p$i_obs[[NROW(p)]] + 10,
    label = "Rb = 1",
    color = "goldenrod3",
    fontface = "bold",
    vjust = 0
  ) +
  # coord_cartesian(ylim = c(0, 2e3)) +
  ylab("Observed Cases")
  # labs(title = "Rb: Implications of Behavior Change")
