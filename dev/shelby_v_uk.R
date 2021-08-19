library(magrittr)
library(ggplot2)
# Case data --------------------------------------------------------------------

# UK
case_uk <- vroom::vroom(
  "https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesBySpecimenDate%22:%22newCasesBySpecimenDate%22,%22cumCasesBySpecimenDate%22:%22cumCasesBySpecimenDate%22%7D&format=csv",
  col_select = c(
    "date",
    cases = "newCasesBySpecimenDate"
  )
) %>%
  dplyr::arrange(date) %>%
  tidyr::complete(
    date = seq(
      min(.[["date"]], na.rm = TRUE),
      max(.[["date"]], na.rm = TRUE),
      by= 1L
    ),
    fill = list(cases = 0L)
  ) %>%
  dplyr::mutate(
    avg = zoo::rollmean(cases, 7L, fill = NA, align = "right"),
    rate = 1e5 * avg / 66785324,
    region = "UK"
  )

# Shelby County
inv <- coviData::pos(coviData::process_inv())
case_sc <- inv %>%
  dplyr::filter(
    as.Date("2020-03-05") <= specimen_coll_dt,
    specimen_coll_dt <= coviData::date_inv()
  ) %>%
  dplyr::count(date = specimen_coll_dt, name = "cases") %>%
  tidyr::complete(
    date = seq(
      min(.[["date"]], na.rm = TRUE),
      max(.[["date"]], na.rm = TRUE),
      by= 1L
    ),
    fill = list(cases = 0L)
  ) %>%
  dplyr::mutate(
    avg = zoo::rollmean(cases, 7L, fill = NA, align = "right"),
    rate = 1e5 * avg / 937166,
    region = "Shelby"
  )

# Join
cases <- dplyr::bind_rows(
  dplyr::semi_join(case_sc, case_uk, by = "date"),
  dplyr::semi_join(case_uk, case_sc, by = "date")
) %>%
  dplyr::arrange(date)

# Align Dates ------------------------------------------------------------------
sc <- dplyr::filter(cases, region == "Shelby", date >= as.Date("2021-06-16"))
uk <- dplyr::filter(cases, region == "UK", date >= as.Date("2021-05-16"))

# Optimization function
get_shift_mae <- function(sc, uk, shift = 0) {
  dplyr::left_join(
    sc,
    dplyr::mutate(uk, date = date + shift),
    by = "date",
    suffix = c("_sc", "_uk")
  ) %>%
    dplyr::summarize(
      mae = mean(abs(rate_sc - rate_uk), na.rm = TRUE),
      diff = tail(rate_sc - rate_uk, 1L)
    )
}

# Optimize MAE of case rates since Delta wave onset
get_min_mae <- function(sc, uk, min_shift = 0, max_shift = 60, plot = c("mae", "diff")) {
  shifts <- seq(min_shift, max_shift, by = 1)
  error <- purrr::map_dfr(shifts, ~ get_shift_mae(sc, uk, shift = .x)) %>%
    dplyr::mutate(shift = shifts) %>%
    dplyr::filter(dplyr::cumall(!is.na(mae)))

  if (any(c("mae", "diff") %in% plot)) {
    plt <- ggplot(
      tidyr::pivot_longer(error, dplyr::all_of(plot), names_to = "metric"),
      aes(x = shift, y = value, color = metric)
    ) +
      geom_point() +
      scale_x_continuous(
        breaks = seq(
          floor(min_shift * 10)/10,
          ceiling(max_shift*10)/10,
          by = 10
        )
      )
    coviData::set_covid_theme(plt) %>%
      coviData::add_axis_labels(xlab = "Shift", ylab = "MAE/Difference") %>%
      show()
  }

  dplyr::filter(error, mae == min(mae, na.rm = TRUE))
}

# What's the best fit? Plot MAE
get_min_mae(sc, uk, plot = "mae")

# Plot final graphic -----------------------------------------------------------
plot_uk_sc <- function(cases, vac = NULL, shift = NULL, plot_error = TRUE) {

  if (is.null(shift)) {
    min_error <- get_min_mae(
      dplyr::filter(cases, region == "Shelby", date >= as.Date("2021-06-16")),
      dplyr::filter(cases, region == "UK", date >= as.Date("2021-05-16")),
      plot = NULL
    )
  }
  if (exists("min_error")) {
    shift <- min_error$shift
    mae  <- round(min_error$mae, 2L)
    diff <- round(min_error$diff, 2L)
  } else {
    error <- get_shift_mae(
      dplyr::filter(cases, region == "Shelby", date >= as.Date("2021-06-16")),
      dplyr::filter(cases, region == "UK", date >= as.Date("2021-05-16")),
      shift = shift
    )
    mae <- round(error$mae, 2L)
    diff <- round(error$diff, 2L)
  }
  if (diff > 0) diff <- paste0("+", diff)

  min_date <- min(coviData::date_inv() - 5 - (90 - 1), as.Date("2021-06-16"))
  gg_data <- dplyr::filter(
    cases,
    {{ min_date }} <= date,
    date <= coviData::date_inv() - 5
  )

  shift_data <- dplyr::mutate(
    dplyr::filter(gg_data, region == "UK"),
    date = date + shift,
    rate = dplyr::if_else(date < as.Date("2021-06-16"), NA_real_, rate)
  )

  diff_data <- dplyr::left_join(
    shift_data,
    dplyr::filter(gg_data,region=="Shelby") %>% dplyr::select(c("date","rate")),
    by = "date",
    suffix = c("_uk", "_sc")
  ) %>%
    dplyr::transmute(date, diff = rate_sc - rate_uk)

  max_date <- max(gg_data[["date"]], na.rm = TRUE)
  uk_rate <- gg_data %>%
    dplyr::filter(region == "UK") %>%
    dplyr::filter(rate == max(rate, na.rm = TRUE)) %>%
    dplyr::pull(rate)
  uk_date <- gg_data %>%
    dplyr::filter(region == "UK", rate == {{ uk_rate }}) %>%
    dplyr::pull(date)
  sc_rate <- gg_data %>%
    dplyr::filter(date == {{max_date}}, region == "Shelby") %>%
    dplyr::pull(rate)

  print(paste("UK Max:", round(uk_rate), "on", uk_date))
  plt <- ggplot(gg_data, aes(x = date, y = rate, color = region)) +
    # Shift arrow
    annotate(
      "segment",
      x = as.Date("2021-06-25"),
      y = gg_data %>%
        dplyr::filter(region == "UK", date == as.Date("2021-06-25")) %>%
        dplyr::pull(rate),
      xend = as.Date("2021-06-25") + shift,
      yend = gg_data %>%
        dplyr::filter(region == "UK", date == as.Date("2021-06-25")) %>%
        dplyr::pull(rate),
      arrow = arrow(),
      size = 1,
      color = "cornflowerblue"
    ) +
    # Shift label
    annotate(
      "label",
      x = as.Date("2021-06-25"),
      y = gg_data %>%
        dplyr::filter(region == "UK", date == as.Date("2021-06-25")) %>%
        dplyr::pull(rate),
      label = paste0("If Shelby follows UK\n(shifted by ", shift, " days)"),
      color = "cornflowerblue",
      size = 14/.pt,
      hjust = 1,
      fill = "#f0f0f0"
    ) +
    # Shifted UK line
    geom_line(
      data = shift_data,
      linetype = "dashed",
      size = 1,
      show.legend = FALSE
    ) +
    # Actual case rate lines
    geom_line(size = 1, show.legend = FALSE) +
    scale_color_manual(
      values = c(UK = "cornflowerblue", Shelby = "firebrick")
    ) +
    # Region labels
    annotate(
      "label",
      x = min(gg_data$date, na.rm = TRUE),
      y = gg_data %>%
        dplyr::filter(date == min(date, na.rm = TRUE)) %>%
        dplyr::pull(rate),
      label = c("Shelby", "UK"),
      color = c("firebrick", "cornflowerblue"),
      size = 14 / .pt,
      fontface = "bold",
      fill = "#f0f0f0",
      hjust = 0
    ) +
    # UK Rate label
    annotate(
      "label",
      x = c(uk_date + shift, max_date),
      y = c(uk_rate, sc_rate),
      label = paste0(round(c(uk_rate, sc_rate)), " cases/100k"),
      color = c("cornflowerblue", "firebrick"),
      fill = "#f0f0f0",
      size = 14/.pt,
      vjust = 0
    )

  # Add error line
  if (plot_error) {
    plt <- plt +
      # Difference in aligned case rates
      geom_line(
        data = diff_data,
        aes(y = diff),
        color = "grey30",
        show.legend = FALSE
      ) +
      # Difference label
      annotate(
        "text",
        x = max_date,
        y = dplyr::filter(diff_data, date == {{max_date}}) %>% dplyr::pull(diff),
        label = paste(
          diff, "case rate difference\n",
          "MAE = ", mae, " cases per 100k per day"
        ),
        color = "grey30",
        hjust = 0
      )
  } else {
    plt <- plt +
      coord_cartesian(ylim = c(0, max(gg_data$rate, na.rm = TRUE)))
  }

  # Theme and add titles/captions
  plt %>%
    coviData::set_covid_theme() %>%
    coviData::add_axis_labels(ylab = "Cases per 100,000") %>%
    coviData::add_scale_month() %>%
    coviData::add_title_caption(
      title = "Case Rates: Shelby County vs UK"
    )
}

plot_uk_sc(cases, shift = 30, plot_error = TRUE)

coviData::save_plot(
  plot_uk_sc(cases, shift = 30, plot_error = FALSE),
  path = paste0(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/uk_fig/",
    "uk_v_shelby_", Sys.Date(), ".png"
  )
)

coviData::save_plot(
  plot_uk_sc(cases, shift = 30, plot_error = TRUE),
  path = paste0(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/uk_fig/w_error/",
    "uk_v_shelby_w_error_", Sys.Date(), ".png"
  )
)
