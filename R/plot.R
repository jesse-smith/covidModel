plot_rt <- function(
  .data,
  lag = 0L,
  start = "2020-04-01",
  end = Sys.Date()
) {

  .data %>%
    ggplot_rt() %>%
    add_rt_level_reference() %>%
    add_rt_event_reference() %>%
    add_rt_curve() %>%
    add_rt_recent() %>%
    add_rt_axis_labels() %>%
    add_rt_title_caption() %>%
    add_rt_scale()
}

ggplot_rt <- function(
  .data,
  start = "2020-04-01",
  end = Sys.Date()
) {

  start <- lubridate::as_date(start)
  end <- lubridate::as_date(end)

  # Decide y axis range based on period of interest (not all data)
  truncated_data <- timetk::filter_by_time(.data, .data[[".t"]], start, end)
  max_span <- max(
    abs(1 - min(truncated_data$.pred_lower, na.rm = TRUE)),
    abs(1 - max(truncated_data$.pred_upper, na.rm = TRUE))
  )

  xlim <- c(start, end)
  ylim <- purrr::when(
    max_span,
    . <= 1 ~ c(min = 0, max = 2),
    ~ c(min = 0, max = 1 + max_span)
  )

  ggplot2::ggplot(.data, ggplot2::aes(x = lubridate::as_date(.data[[".t"]]))) +
    ggthemes::theme_fivethirtyeight(base_size = 14L) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
}

add_rt_level_reference <- function(gg_obj) {

  gg_obj +
    # Low risk reference
    ggplot2::annotate(
      "rect",
      xmin = lubridate::as_date("2010-01-01"),
      ymin = -1,
      xmax = lubridate::as_date("2030-01-01"),
      ymax = 0.9,
      fill = "#00d474",
      alpha = 0.5
    ) +
    # Moderate risk reference
    ggplot2::annotate(
      "rect",
      xmin = lubridate::as_date("2010-01-01"),
      ymin = 0.9,
      xmax = lubridate::as_date("2030-01-01"),
      ymax = 1.1,
      fill = "#ffc900",
      alpha = 0.5
    ) +
    # High risk reference
    ggplot2::annotate(
      "rect",
      xmin = lubridate::as_date("2010-01-01"),
      ymin = 1.1,
      xmax = lubridate::as_date("2030-01-01"),
      ymax = 1.4,
      fill = "#ff9600",
      alpha = 0.5
    ) +
    # Severe risk reference
    ggplot2::annotate(
      "rect",
      xmin = lubridate::as_date("2010-01-01"),
      ymin = 1.4,
      xmax = lubridate::as_date("2030-01-01"),
      ymax = 4,
      fill = "#FF0034",
      alpha = 0.5
    ) +
    # Reference line: Rt = 1
    ggplot2::geom_hline(
      yintercept = 1,
      color = "grey30",
      size = 0.75
    )
}

add_rt_curve <- function(gg_obj) {

  gg_obj +
    # Add 95% CI
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data[[".pred_lower"]],
        ymax = .data[[".pred_upper"]]
      ),
      fill = "grey30",
      linetype = "blank",
      alpha = 1/3
    ) +
    # Add line
    ggplot2::geom_line(
      ggplot2::aes(y = .data[[".pred"]]),
      color = "grey30",
      alpha = 2/3,
      size = 1,
      show.legend = FALSE
    ) +
    # Add points
    ggplot2::geom_point(
      ggplot2::aes(y = .data[[".pred"]]),
      color = "grey30",
      size = 1,
      show.legend = FALSE
    )
}

add_event <- function(
  gg_obj,
  date,
  lab,
  hjust = 0,
  vjust = 0,
  angle = -90,
  face = c("bold", "italic", "bold.italic", "plain"),
  line = c("dashed", "solid", "blank", "dotted")
) {

  date <- lubridate::as_date(date)

  face <- rlang::arg_match(face)[[1]]

  line <- rlang::arg_match(line)[[1]]

  gg_obj +
    ggplot2::geom_vline(
      xintercept = date,
      linetype = line,
      color = "grey30",
      size = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = if (hjust <= 0.5) date + 1 else date - 1,
      y = 2,
      label = lab,
      hjust = hjust,
      vjust = vjust,
      angle = angle,
      fontface = face,
      color = "grey30"
    )
}

add_today <- function(gg_obj) {
  gg_obj %>%
    add_event(
      lubridate::today(),
      "Today",
      angle = 0,
      vjust = 1,
      face = "plain",
      line = "dotted"
    )
}

add_rt_event_reference <- function(gg_obj) {
  gg_obj %>%
    add_today() %>%
    add_event("2020-03-24", "Safer-at-Home") %>%
    add_event("2020-05-04", "Phase 1 Reopen") %>%
    add_event("2020-05-18", "Phase 2 Reopen") %>%
    add_event("2020-07-08", "Bars Close\nMask Directive", vjust = 0.6) %>%
    add_event("2020-09-22", "Bars Reopen") %>%
    add_event("2020-09-29", "End State Restrictions") %>%
    add_event(
      "2020-11-23",
      "Emphasize Masking\nReduce Dining Hours & Capacity",
      vjust = 0.6
    )
}

add_rt_recent <- function(gg_obj) {

  rt_recent <- gg_obj[["data"]] %>%
    dplyr::filter(.data[[".t"]] == max(.data[[".t"]], na.rm = TRUE))

  color <- purrr::when(
    rt_recent[[".pred"]],
    . < 0.9 ~ "#00d474",
    . < 1.1 ~ "#ffc900",
    . < 1.4 ~ "#ff9600",
    ~ "#FF0034"
  )

  fill <- purrr::when(
    color,
    color == "#ffc900" ~ "grey30",
    ~ "#f0f0f0"
  )

  label <- paste0(
    format(rt_recent[[".t"]], "%b %d"), "\n",
    round(rt_recent[[".pred"]], 2L),
    " (",
    round(rt_recent[[".pred_lower"]], 2L), " - ",
    round(rt_recent[[".pred_upper"]], 2L),
    ")"
  )

  gg_obj +
    # Line from label to last point
    ggplot2::annotate(
      "segment",
      x = rt_recent[[".t"]],
      y = rt_recent[[".pred"]],
      xend = rt_recent[[".t"]],
      yend = max(0, rt_recent[[".pred"]] - 1/3),
      color = color,
      size = 1
    ) +
    # Label
    ggplot2::annotate(
      "label",
      x = rt_recent[[".t"]],
      y = max(0, rt_recent[[".pred"]] - 1/3),
      label = label,
      vjust = 1,
      color = color,
      fill = fill,
      fontface = "bold",
      label.size = 1
    ) +
    # Colored Rt point
    ggplot2::annotate(
      "point",
      x = rt_recent[[".t"]],
      y = rt_recent[[".pred"]],
      color = color,
      size = 1.5
    ) +
    # Colored label point
    ggplot2::annotate(
      "point",
      x = rt_recent[[".t"]],
      y = max(0, rt_recent[[".pred"]] - 1/3),
      color = color,
      size = 1.5
    )
}

add_rt_axis_labels <- function(gg_obj) {
  gg_obj +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Rt") +
    ggplot2::theme(axis.title = ggplot2::element_text())
}

add_rt_title_caption <- function(gg_obj) {
  gg_obj +
    ggplot2::labs(
      title = "Effective Reproduction Number (Rt)",
      subtitle = "Estimated from confirmed cases by specimen collection date",
      caption = "Source: Joint Task Force ESF-8 Data Subcommittee"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 16, hjust = 0.5),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        size = 12,
        face = "italic"
      )
    )
}

add_rt_scale <- function(gg_obj) {
  gg_obj +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%B") +
    ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2))
}

save_plot <- function(gg_obj = ggplot2::last_plot(), path, hd_pct = 1) {

  coviData:::path_create(path)

  ggplot2::ggsave(
    filename = coviData:::path_create(path),
    plot = gg_obj,
    width = 16 * hd_pct,
    height = 9 * hd_pct
  )
}
