library(ggplot2)
library(magrittr)
library(covidModel)

# UK ---------------------------------------------------------------------------
uk <- dplyr::mutate(
  sirv(
    active_cases = 500,
    end = "2021-06-01",
    rt = 1.05,
    pct_var = 0.1/0.85
  ),
  `Initial % UK` = 0.1
)
# UK Variant: Proportion Over Time ---------------------------------------------
purrr::map_dfr(
  c(0.01, 0.05, 0.15, 0.2, 0.25, 0.3),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500,
      end = "2021-06-01",
      rt = 1.05,
      pct_var = .x/0.85
    ),
    `Initial % UK` = .x
  ) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["pct_uk"]], group = .data[["Initial % UK"]]) +
  geom_line(data = uk, aes(x = time, y = pct_uk), color = "goldenrod3", size = 1.5) +
  annotate(
    "text",
    x = Sys.Date(),
    y = c(0.01, 0.05, 0.15, 0.2, 0.25, 0.3),
    label = c("1%", "5%", "15%", "20%", "25%", "30%"),
    hjust = 1,
    color = "grey30",
    fontface = "bold",
    size = 4.5
  ) +
  annotate(
    "text",
    x = Sys.Date(),
    y = 0.1,
    label = "10%",
    hjust = 1,
    color = "goldenrod3",
    fontface = "bold",
    size = 4.5
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_continuous(breaks = c(0.1, 0.2, 0.3), labels = scales::percent) +
  ylab("% UK Variant") +
  labs(title = "UK Variant: Proportion Over Time", subtitle = "Initial Rt = 1.05, Vaccinations = 3500/day") +
  theme(legend.position = "right", legend.direction = "vertical")

# Rb ---------------------------------------------------------------------------
rt_uk <- dplyr::mutate(
  sirv(
    active_cases = 500,
    start = Sys.Date() - 1L,
    end = "2021-06-01",
    rt = 1
  ),
  `Initial Rt` = 1
) %>%
  dplyr::mutate(`UK Variant` = "Present")

rt_no_uk <- dplyr::mutate(
  sirv(
    active_cases = 500,
    start = Sys.Date() - 1L,
    end = "2021-06-01",
    rt = 1,
    pct_var_uk = 0
  ),
  `Initial Rt` = 1
) %>%
  dplyr::mutate(`UK Variant` = "Absent")

rt <- dplyr::bind_rows(rt_no_uk, rt_uk)

# Rt: Effects of Behavior on Observed Cases (all) ------------------------------
wo_uk <- purrr::map_dfr(
  seq(0.8, 1.2, by = 0.05),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500/.x,
      start = Sys.Date() - 1L,
      end = "2021-06-01",
      pct_var_uk = 0,
      rt = .x
    ),
    `Initial Rt` = .x
  ) %T>% {cat("\n")}
) %>%
  dplyr::mutate(`UK Variant` = "Absent")

w_uk <- purrr::map_dfr(
  seq(0.8, 1.2, by = 0.05),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500/.x,
      start = Sys.Date() - 1L,
      end = "2021-06-01",
      rt = .x
    ),
    `Initial Rt` = .x
  ) %T>% {cat("\n")}
) %>%
  dplyr::mutate(`UK Variant` = "Present")

uk <- dplyr::bind_rows(wo_uk, w_uk)

w_uk %>%
  plot_sirv(.y = .data[["i_obs"]], group = .data[["Initial Rt"]]) +
  geom_line(data = rt_uk, aes(x = .data[["time"]], y = .data[["i_obs"]]), color = "goldenrod3", size = 1.5) +
  coord_cartesian(ylim = c(0, 1000)) +
  scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
  scale_color_continuous(breaks = seq(0.8, 1.2, by = 0.1)) +
  labs(
    title = "Behavior: Effects on Observed Cases (w/ UK Variant)",
    subtitle = "Vaccinations = 3500/day, Initial % UK = 10%"
  ) +
  theme(legend.position = "right", legend.direction = "vertical")
wo_uk %>%
  plot_sirv(.y = .data[["i_obs"]], group = .data[["Initial Rt"]]) +
  geom_line(data = rt_no_uk, aes(x = .data[["time"]], y = .data[["i_obs"]]), color = "goldenrod3", size = 1.5) +
  coord_cartesian(ylim = c(0, 1000)) +
  scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
  scale_color_continuous(breaks = seq(0.8, 1.2, by = 0.1)) +
  labs(
    title = "Behavior: Effects on Observed Cases (w/o UK Variant)",
    subtitle = "Vaccinations = 3500/day, Initial % UK = 0%"
  ) +
  theme(legend.position = "right", legend.direction = "vertical")


gganimate::animate(plt, device = "svglite")
gganimate::anim_save("test.gif", plt, width = 16, height = 9, device = "svglite")

# Vaccinations
v <- dplyr::mutate(
  sirv(
    active_cases = 500,
    start = Sys.Date() - 1L,
    end = "2021-06-01",
    rt = 1.05,
    pct_var = 0.1/0.85
  ),
  `Daily Vaccinations` = 3500
)

# Vaccination: Effects on Observed Cases (all)
purrr::map_dfr(
  c(2000, 5000),
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
  coord_cartesian(ylim = c(0, 1e3)) +
  ylab("Observed Cases") +
  labs(title = "Vaccination: Effects on Observed Cases", subtitle = "Initial Rt = 1.05, Initial % UK = 10%") +
  scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
  scale_color_continuous(breaks = c(2000, 3500, 5000)) +
  theme(legend.position = "right", legend.direction = "vertical")
