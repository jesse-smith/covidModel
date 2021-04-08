library(ggplot2)
library(magrittr)
library(covidModel)

sirv_p1 <- function(...) {

  checkmate::assert_names(names(rlang::list2(...)), disjunct.from = "pct_p1")

  dplyr::bind_rows(
    dplyr::mutate(sirv(pct_p1 = 0.005, ...), `Initial % P.1` = 0.005),
    dplyr::mutate(sirv(pct_p1 = 0.03, ...), `Initial % P.1` = 0.03)
  )
}

sirv_uk <- function(...) {

  checkmate::assert_names(names(rlang::list2(...)), disjunct.from = "pct_uk")

  dplyr::bind_rows(
    dplyr::mutate(sirv(pct_uk = 0.1, ...), `Initial % UK` = 0.1),
    dplyr::mutate(sirv(pct_uk = 0.7, ...), `Initial % UK` = 0.7)
  )
}

# UK ---------------------------------------------------------------------------
d_uk <- purrr::map_dfr(
  seq(0.1, 0.7, by = 0.05),
  ~ dplyr::mutate(
    sirv_p1(start = as.Date("2021-03-28"), end = as.Date("2021-06-01"), pct_uk = .x),
    `Initial % UK` = .x
  )
) %>%
  dplyr::arrange(`Initial % P.1`, `Initial % UK`, time)

# UK Variant: Proportion Over Time ---------------------------------------------

d_uk %>%
  plot_sirv0(.y = .data[["pct_uk"]], group = .data[["Initial % UK"]]) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_continuous(labels = scales::percent) +
  ylab("% UK Variant") +
  gganimate::transition_states(.data[["Initial % P.1"]], transition_length = 0.5) +
  gganimate::ease_aes('cubic-in-out') +
  labs(title = "UK Variant: Proportion Over Time") +
  theme(legend.position = "right", legend.direction = "vertical")

# UK Variant: Effects on Observed Cases ----------------------------------------
d_uk %>%
  {plot_sirv0(., .y = .data[["i_obs"]], group = .data[["Initial % UK"]]) +
  coord_cartesian(ylim = c(0, 1e3)) +
  scale_color_continuous(labels = scales::percent) +
  ylab("Observed Cases") +
  gganimate::transition_states(.data[["Initial % P.1"]], transition_length = 0.5) +
  gganimate::ease_aes('cubic-in-out') +
  labs(title = "UK Variant: Effects on Observed Cases") +
  theme(legend.position = "right", legend.direction = "vertical")} %>%
  gganimate::animate(width = 1280, height = 720)

# P.1
d_p1 <- purrr::map_dfr(
  seq(0.005, 0.03, by = 0.005),
  ~ dplyr::mutate(
    sirv_uk(start = as.Date("2021-03-28"), end = as.Date("2021-06-01"), pct_p1 = .x),
    `Initial % P.1` = .x
  )
) %>%
  dplyr::arrange(`Initial % UK`, `Initial % P.1`, time)

# P.1 Variant: Proportion Over Time ---------------------------------------------

d_p1 %>%
  plot_sirv0(.y = .data[["pct_p1"]], group = .data[["Initial % P.1"]]) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_continuous(labels = scales::percent) +
  ylab("% P.1 Variant") +
  gganimate::transition_states(.data[["Initial % UK"]], transition_length = 0.5) +
  gganimate::ease_aes('cubic-in-out') +
  labs(title = "P.1 Variant: Proportion Over Time") +
  theme(legend.position = "right", legend.direction = "vertical")


# P.1 Variant: Observed Cases --------------------------------------------
d_p1 %>%
  {plot_sirv0(., .y = .data[["i_obs"]], group = .data[["Initial % P.1"]]) +
  coord_cartesian(ylim = c(0, 1e3)) +
  scale_color_continuous(labels = scales::percent) +
  ylab("Observed Cases") +
  gganimate::transition_states(.data[["Initial % UK"]], transition_length = 0.5) +
  gganimate::ease_aes('cubic-in-out') +
  labs(title = "P.1 Variant: Effects on Observed Cases") +
  theme(legend.position = "right", legend.direction = "vertical")} %>%
  gganimate::animate(width = 1280, height = 720)


# Vaccination: Effects on P.1 Variant
purrr::map_dfr(
  c(seq(5e2, 3e3, by = 5e2), seq(4e3, 5e3, by = 5e2)),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500,
      end = "2021-06-01",
      rt = 1.05,
      pct_var = 0.1/0.85,
      vac_per_day = .x
    ),
    `Daily Vaccinations` = .x
  ) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["pct_uk"]], group = .data[["Daily Vaccinations"]]) +
  geom_line(data = v, aes(x = time, y = pct_uk), color = "goldenrod3", size = 1.5) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("% UK Variant") +
  labs(title = "Vaccination: Effects on UK Variant") +
  theme(legend.position = "right", legend.direction = "vertical")

# Vaccination: Effects on Observed Cases (zoomed)
purrr::map_dfr(
  c(seq(5e2, 3e3, by = 5e2), seq(4e3, 5e3, by = 5e2)),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500,
      end = "2021-06-01",
      rt = 1.05,
      pct_var = 0.1/0.85,
      vac_per_day = .x
    ),
    `Daily Vaccinations` = .x
  ) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["i_obs"]], group = .data[["Daily Vaccinations"]]) +
  geom_line(data = v, aes(x = time, y = i_obs), color = "goldenrod3", size = 1.5) +
  coord_cartesian(ylim = c(0, 5e2)) +
  ylab("Observed Cases") +
  labs(title = "Vaccination: Effects on Observed Cases") +
  theme(legend.position = "right", legend.direction = "vertical")

# Vaccination: Effects on Observed Cases (all)
purrr::map_dfr(
  c(seq(5e2, 3.4e3, by = 1e2), seq(3.6e3, 5e3, by = 1e2)),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500,
      end = "2021-06-01",
      rt = 1.05,
      pct_var = 0.1/0.85,
      vac_per_day = .x
    ),
    `Daily Vaccinations` = .x
  ) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["i_obs"]], group = .data[["Daily Vaccinations"]]) +
  geom_line(data = v, aes(x = time, y = i_obs), color = "goldenrod3", size = 1.5) +
  coord_cartesian(ylim = c(0, 2e3)) +
  ylab("Observed Cases") +
  labs(title = "Vaccination: Effects on Observed Cases") +
  theme(legend.position = "right", legend.direction = "vertical")
