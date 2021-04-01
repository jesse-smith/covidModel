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
  ylab("% UK Variant") +
  labs(title = "UK Variant: Proportion Over Time") +
  theme(legend.position = "right", legend.direction = "vertical")

# UK Variant: Effects on Observed Cases ----------------------------------------
purrr::map_dfr(
  c(seq(0.01, 0.095, by = 0.005), seq(0.105, 0.3, by = 0.005)),
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
  plot_sirv(.y = .data[["i_obs"]], group = .data[["Initial % UK"]]) +
  geom_line(data = uk, aes(x = time, y = i_obs), color = "goldenrod3", size = 1.5) +
  coord_cartesian(ylim = c(0, 500)) +
  scale_color_continuous(labels = scales::percent) +
  labs(title = "UK Variant: Effects on Observed Cases") +
  theme(legend.position = "right", legend.direction = "vertical")


# Rb ---------------------------------------------------------------------------
rb <- dplyr::mutate(
  sirv(
    active_cases = 500,
    end = "2021-06-01",
    rt = 1.05,
    pct_var = 0.1/0.85
  ),
  `Rb` = 1.00
)

# Rb: Effects of Behavior on UK Variant
purrr::map_dfr(
  c(seq(0.7, 1, by = 0.05), seq(1.05, 1.3, by = 0.05)),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500/.x,
      end = "2021-06-01",
      r0 = .x,
      pct_var = 0.1/0.85
    ),
    `Rb` = .x
  ) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["pct_uk"]], group = .data[["Rb"]]) +
  geom_line(data = rb, aes(x = time, y = pct_uk), color = "goldenrod3", size = 1.5) +
coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("% UK Variant") +
  labs(title = "Behavior: Effects on UK Variant") +
  theme(legend.position = "right", legend.direction = "vertical")

# Rb: Effects of Behavior on Observed Cases (w/o UK Variant)
rb_no_uk <- dplyr::mutate(
  sirv(
    active_cases = 500,
    end = "2021-06-01",
    r0 = 1,
    pct_var = 0
  ),
  `Rt` = 1.00
)
purrr::map_dfr(
  c(seq(0.7, 1, by = 0.05), seq(1.05, 1.3, by = 0.05)),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500/.x,
      end = "2021-06-01",
      r0 = .x,
      pct_var = 0
    ),
    `Rt` = .x
  ) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["i_obs"]], group = .data[["Rt"]]) +
  geom_line(data = rb_no_uk, aes(x = time, y = i_obs), color = "goldenrod3", size = 1.5) +
  coord_cartesian(ylim = c(0, 500)) +
  labs(title = "Behavior: Effects on Observed Cases (w/o UK Variant)") +
  theme(legend.position = "right", legend.direction = "vertical")

# Rb: Effects of Behavior on Observed Cases (zoomed) ---------------------------
purrr::map_dfr(
  c(seq(0.7, 0.95, by = 0.05), seq(1.05, 1.3, by = 0.05)),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500/.x,
      end = "2021-06-01",
      r0 = .x
    ),
    `Rb` = .x
  ) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["i_obs"]], group = .data[["Rb"]]) +
  geom_line(data = rb, aes(x = time, y = i_obs), color = "goldenrod3", size = 1.5) +
  annotate(
    "text",
    x = as.Date("2021-06-01"),
    y = rb$i_obs[[NROW(rb)]] + 5,
    label = "Rb = 1",
    vjust = 0,
    color = "goldenrod3",
    fontface = "bold",
    size = 4.5
  ) +
  coord_cartesian(ylim = c(0, 500)) +
  labs(title = "Behavior: Effects on Observed Cases (w/ UK Variant)") +
  theme(legend.position = "right", legend.direction = "vertical")

# Rb: Effects of Behavior on Observed Cases (all) ------------------------------
purrr::map_dfr(
  c(seq(0.7, 0.99, by = 0.01), seq(1.01, 1.3, by = 0.01)),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500/.x,
      end = "2021-06-01",
      r0 = .x
    ),
    `Rb` = .x
  ) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["i_obs"]], group = .data[["Rb"]]) +
  geom_line(data = rb, aes(x = time, y = i_obs), color = "goldenrod3", size = 1.5) +
  coord_cartesian(ylim = c(0, 3500)) +
  labs(title = "Behavior: Effects on Observed Cases (w/ UK Variant)") +
  theme(legend.position = "right", legend.direction = "vertical")

# Vaccinations
v <- dplyr::mutate(
  sirv(
    active_cases = 500,
    end = "2021-06-01",
    rt = 1.05,
    pct_var = 0.1/0.85
  ),
  `Daily Vaccinations` = 3500
)

# Vaccination: Effects on UK Variant
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

# What about vaccine resistance?
ek_no_uk <- dplyr::mutate(
  sirv(
    active_cases = 500,
    end = "2021-06-01",
    rt = 1.05,
    pct_var = 0.01,
    pct_var_uk = 0,
    pct_var_ek = 1
  ),
  `Initial % Resistant` = 0.01
)
purrr::map_dfr(
  seq(0.02, 0.15, by = 0.01),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500,
      end = "2021-06-01",
      rt = 1.05,
      pct_var = .x,
      pct_var_ek = 1,
      pct_var_uk = 0
    ),
    `Initial % Resistant` = .x
  ) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["pct_ek"]], group = .data[["Initial % Resistant"]]) +
  geom_line(data = ek_no_uk, aes(x = time, y = pct_ek), color = "goldenrod3", size = 1.5) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("% Resistant Variant") +
  labs(title = "Resistant Variant: Proportion Over Time (w/o UK Variant)") +
  theme(legend.position = "right", legend.direction = "vertical")

ek <- dplyr::mutate(
  sirv(
    active_cases = 500,
    end = "2021-06-01",
    rt = 1.05,
    pct_var = 0.1/0.85,
    pct_var_uk = 0.85,
    pct_var_ek = 0.01
  ),
  `Initial % Resistant` = 0.01
)
purrr::map_dfr(
  seq(0.02, 0.15, by = 0.01),
  ~ dplyr::mutate(
    sirv(
      active_cases = 500,
      end = "2021-06-01",
      rt = 1.05,
      pct_var = 0.1/0.85,
      pct_var_ek = .x,
      pct_var_uk = 0.85
    ),
    `Initial % Resistant` = .x
  ) %T>% {cat("\n")}
) %>%
  plot_sirv(.y = .data[["pct_ek"]], group = .data[["Initial % Resistant"]]) +
  geom_line(data = ek, aes(x = time, y = pct_ek), color = "goldenrod3", size = 1.5) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("% Resistant Variant") +
  labs(title = "Resistant Variant: Proportion Over Time (w/ UK Variant)") +
  theme(legend.position = "right", legend.direction = "vertical")
