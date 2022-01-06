#' Plot the Results of `estimate_rt()`
#'
#' `plot_rt()` plots the effective reproductive number over the course of the
#' epidemic in Shelby County, along with reference dates and severity levels
#' for comparison. Optionally, supplied unsmoothed Rt estimates to `.rough_rt`
#' will add points for this estimates to the background for reference.
#'
#' @param .data Rt estimates resulting from a call to
#'   \code{\link[covidModel:estimate_rt]{estimate_rt()}}
#'
#' @param start The start date of the x axis
#'
#' @param end The end date of the x axis
#'
#' @param .rough_rt Unsmoothed Rt estimates, e.g. as produced by
#'   `estimate_rt(trend = 1L, boost = FALSE)`
#'
#' @return A `ggplot` object
#'
#' @export
plot_rt <- function(
  .data,
  start = "2020-04-01",
  end = Sys.Date(),
  .rough_rt = NULL
) {

  .data %>%
    ggplot_rt() %>%
    add_rt_level_reference() %>%
    {if (!is.null(.rough_rt)) add_rt_rough(., .rough_rt = .rough_rt) else .} %>%
    add_rt_interval() %>%
    add_rt_curve() %>%
    add_covid_events(lab_y = 2.5) %>%
    add_rt_recent() %>%
    add_rt_axis_labels() %>%
    add_rt_title_caption() %>%
    add_rt_scale()
}

#' Initialize Plotting of `estimate_rt()` Results
#'
#' @param .data The result of `estimate_rt()`
#'
#' @param start The start date of the figure
#'
#' @param end  THe end date of the figure
#'
#' @return A `ggplot` object
#'
#' @noRd
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
    . <= 1 ~ c(min = 0, max = 2.5),
    ~ c(min = 0, max = 1 + max_span)
  )

  ggplot2::ggplot(
    .data,
    ggplot2::aes(x = lubridate::as_date(.data[[".t"]]))
  ) %>%
    set_covid_theme() %>%
    set_axis_limits(xlim = xlim, ylim = ylim)
}

#' Add Background Reference Colors to Rt Plot
#'
#' @param gg_obj A `ggplot` object returned from `ggplot_rt`
#'
#' @noRd
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

#' Add Prediction Intervals to an Rt Plot
#'
#' @inheritParams add_rt_level_reference
#'
#' @noRd
add_rt_interval <- function(gg_obj) {
  gg_obj +
    # Add 95% CI
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data[[".pred_lower"]],
        ymax = pmin(.data[[".pred_upper"]], 2.5)
      ),
      fill = "grey30",
      linetype = "blank",
      alpha = 1/3
    )
}

#' Add the Expected Value to an Rt Plot
#'
#' @inheritParams add_rt_level_reference
#'
#' @noRd
add_rt_curve <- function(gg_obj) {

  gg_obj +
    # Add line
    ggplot2::geom_line(
      ggplot2::aes(y = .data[[".pred"]]),
      color = "grey30",
      alpha = 2/3,
      size = 1,
      show.legend = FALSE
    )
}

#' Add a Label to the Most Recent Rt Estimate in an Rt Plot
#'
#' @inheritParams add_rt_level_reference
#'
#' @noRd
add_rt_recent <- function(gg_obj) {

  rt_recent <- gg_obj[["data"]] %>%
    dplyr::filter(.data[[".t"]] == max(.data[[".t"]], na.rm = TRUE))

  color <- dplyr::case_when(
    rt_recent[[".pred"]] < 0.9 ~ "#00d474",
    rt_recent[[".pred"]] < 1.1 ~ "#ffc900",
    rt_recent[[".pred"]] < 1.4 ~ "#ff9600",
    rt_recent[[".pred"]] >= 1.4 ~ "#FF0034"
  )

  fill <- purrr::when(
    color,
    . == "#ffc900" ~ "grey30",
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

#' Add Axis Labels to an Rt Plot
#'
#' @inheritParams add_rt_level_reference
#'
#' @noRd
add_rt_axis_labels <- function(gg_obj) {
  add_axis_labels(gg_obj, xlab = "Date", ylab = "Rt")
}

add_rt_title_caption <- function(gg_obj) {
  add_title_caption(
    gg_obj,
    title = "Effective Reproduction Number (Rt)",
    subtitle = "Estimated from confirmed cases by specimen collection date",
    caption = "Source: Joint Task Force ESF-8 Data Subcommittee"
  )
}

#' Add a Monthly Scale to an Rt Plot
#'
#' @inheritParams add_rt_level_reference
#'
#' @noRd
add_rt_scale <- function(gg_obj) {
  add_scale_month(gg_obj) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5))
}

#' Add Points for Unsmoothed Rt Estimates to an Rt Plot
#'
#' @inheritParams add_rt_level_reference
#'
#' @param .rough_rt The unsmoothed Rt estimates, as resulting from
#'   \code{
#'   \link[covidModel:estimate_rt]{estimate_rt(trend = 1L, boost = FALSE)}
#'   }
#'
#' @noRd
add_rt_rough <- function(
  gg_obj,
  .rough_rt
) {

  raw <- dplyr::semi_join(
    .rough_rt,
    gg_obj[["data"]],
    by = ".t"
  )

  gg_obj +
    # Add points
    ggplot2::geom_point(
      ggplot2::aes(y = raw[[".pred"]]),
      color = "grey30",
      alpha = 0.1,
      size = 1,
      show.legend = FALSE
    )
}
