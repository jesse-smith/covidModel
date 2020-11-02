limited_data <- load_limited(file_name = "status_extended.xlsx") %>%
  coviData::preprocess()

limited_data %>%
  dplyr::select(date, positive = spcol_positive, negative = spcol_negative) %>%
  dplyr::mutate(tests = positive + negative) %>%
  dplyr::filter(dplyr::between(date, as.Date("2020-04-01"), max(date) - 5)) %>%
  dplyr::mutate(
    positive = pmax(1, positive),
    negative = pmax(1, negative)
  ) %>%
  na.contiguous() %$%
  positive %>%
  tidyr::replace_na(replace = 0) %>%
  cumsum()
  culogit(max = sqrt(938000))
  model_cases(
    positive,
    dates = date,
    niter = 1100
  ) ->
model



model$log.likelihood %>%
  tibble::as_tibble() %>%
  dplyr::slice_tail(n = 1000) %>%
  .[[1]] %>%
  exp() %>%
  {if (any(. == 0)) rep(1, NROW(.)) else . / max(.)} ->
  lik

model$one.step.prediction.errors %>%
  tibble::as_tibble() %>%
  dplyr::slice_tail(n = 1000) %>%
  abs() %>%
  dplyr::summarize(dplyr::across(.fns = ~ weighted.mean(.x, w = lik))) %>%
  as.matrix() %>%
  mean() ->
sigma

model$state.contributions %>%
  apply(MARGIN = c(1,3), FUN = sum, na.rm = TRUE) %>%
  tibble::as_tibble() %>%
  dplyr::slice_tail(n = 1000) %>%
  dplyr::summarize(dplyr::across(.fns = ~ weighted.mean(.x, w = lik))) %>%
  tidyr::pivot_longer(cols = dplyr::everything()) %>%
  .[["value"]] ->
  trend

model %>% predict(h = 42, quantiles = c(0.025, 0.25, 0.475, 0.525, 0.75, 0.975)) %$%
  tibble::tibble(
    obs   = NA_real_,
    trend = median,
    lower.95 = interval[1,],
    lower.50 = interval[2,],
    lower.05 = interval[3,],
    upper.05 = interval[4,],
    upper.50 = interval[5,],
    upper.95 = interval[6,]
  ) %>%
  {.^2} %>%
  dplyr::mutate(
    date = seq(max(model$timestamp.info$timestamps) + 1, max(model$timestamp.info$timestamps) + 42, by = 1),
    .before = 1
  ) ->
  predictions

material <- ggsci::pal_material(palette = "blue", n = 4, reverse = TRUE)

tibble::tibble(
  obs   = as.vector(model$original.series),
  trend = trend,
  lower.95 = qnorm(p = 0.025, mean = trend, sd = sigma),
  lower.50 = qnorm(p = 0.25,  mean = trend, sd = sigma),
  lower.05 = c(rep(NA_real_, length(trend)-1), trend[length(trend)]),
  upper.05 = c(rep(NA_real_, length(trend)-1), trend[length(trend)]),
  upper.50 = qnorm(p = 0.75,  mean = trend, sd = sigma),
  upper.95 = qnorm(p = 0.975, mean = trend, sd = sigma)
) %>%
  {.^2} %>%
  dplyr::mutate(trend = c(rep(NA_real_, length(trend)-1), obs[length(trend)])) %>%
  dplyr::mutate(
    date = model$timestamp.info$timestamps,
    .before = 1
  ) ->
  observations

observations %>%
  tibble::add_row(
    predictions
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = date)) +
  ggplot2::geom_hline(yintercept = predictions$trend[[NROW(predictions)]], color = material(1), size = 0.75, linetype = "dotted") +
  ggplot2::geom_hline(yintercept = observations$obs[[NROW(observations)]], color = "gray23", size = 0.75, linetype = "dashed") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower.95, ymax = upper.95, fill = "95%"), alpha = 1/3) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower.50, ymax = upper.50, fill = "50%"), alpha = 1/3) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower.05, ymax = upper.05, fill = "5%")) +
  ggplot2::geom_line(ggplot2::aes(y = obs), color = "gray23") +
  ggplot2::geom_line(ggplot2::aes(y = trend), color = material(1)) +
  ggthemes::theme_fivethirtyeight(base_size = 20) +
  ggplot2::labs(title = "Shelby County Hospital Census", caption = "Data Source: Healthcare Resource Tracking System (HRTS)") +
  ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  # ggplot2::scale_y_continuous(breaks = seq(0, 1000, by = 50), labels = scales::label_comma(suffix = " patients")) +
  # ggplot2::coord_cartesian(xlim = c(as.Date("2020-04-01"), max(predictions$date) + 1), ylim = c(0, max(predictions$upper.95))) +
  ggplot2::scale_color_manual(
    name = "Prediction Confidence:",
    values = material(3),
    aesthetics = c("color", "fill")
  ) +
  # ggplot2::geom_hline(yintercept = 0, color = "grey", size = 1.5) +
  # ggplot2::annotate(
  #   "text",
  #   x = predictions$date[NROW(predictions)] + 0.5,
  #   y = c(predictions$lower.95[NROW(predictions)], predictions$upper.95[NROW(predictions)]),
  #   label = c(predictions$lower.95[NROW(predictions)], predictions$upper.95[NROW(predictions)]) %>% round(),
  #   hjust = 0,
  #   vjust = 0.5,
  #   color = material(3)[[3]],
  #   fontface = "bold",
  #   alpha = 0.5,
  #   size = 4.75
  # ) +
  # ggplot2::annotate(
  #   "text",
  #   x = predictions$date[NROW(predictions)] + 0.5,
  #   y = c(predictions$lower.50[NROW(predictions)], predictions$upper.50[NROW(predictions)]),
  #   label = c(predictions$lower.50[NROW(predictions)], predictions$upper.50[NROW(predictions)]) %>% round(),
  #   hjust = 0,
  #   vjust = 0.5,
  #   color = material(2)[[2]],
  #   fontface = "bold",
  #   alpha = 2/3,
  #   size = 4.75
  # ) +
  # ggplot2::annotate(
  #   "text",
  #   x = predictions$date[NROW(predictions)] + 0.5,
  #   y = c(predictions$lower.05[NROW(predictions)], predictions$upper.05[NROW(predictions)]),
  #   label = c(predictions$lower.05[NROW(predictions)], predictions$upper.05[NROW(predictions)]) %>% round(),
  #   hjust = 0,
  #   vjust = c(1,0),
  #   color = material(1),
  #   fontface = "bold",
  #   size = 4.75
  # ) +
  # ggplot2::annotate(
  #   "label",
  #   x = predictions$date[[1]] - 1,
  #   y = predictions$trend[[NROW(predictions)]] + min(observations$lower.95[(NROW(observations) - 30):NROW(observations)], predictions$lower.95),
  #   label = paste0("30-Day Estimate:\n", round(predictions$trend[[NROW(predictions)]]), " on ", format(predictions$date[[NROW(predictions)]], "%b %d")),
  #   hjust = 0.5,
  #   vjust = 0,
  #   color = material(1),
  #   fontface = "bold",
  #   fill = "#F0F0F0",
  #   size = 4.75
  # ) +
  # ggplot2::annotate(
  #   "segment",
  #   x = predictions$date[[1]] - 1,
  #   y = predictions$trend[[NROW(predictions)]] + min(observations$lower.95[(NROW(observations) - 30):NROW(observations)], predictions$lower.95),
  #   xend = predictions$date[[1]] - 1,
  #   yend = predictions$trend[[NROW(predictions)]],
  #   color = material(1)
  # ) +
  # ggplot2::annotate(
  #   "point",
  #   x = predictions$date[[1]] - 1,
  #   y = predictions$trend[[NROW(predictions)]],
  #   color = material(1)
  # ) +
  # ggplot2::annotate(
  #   "label",
  #   x = observations$date[[NROW(observations)]],
  #   y = min(observations$lower.95[(NROW(observations) - 30):NROW(observations)], predictions$lower.95),
  #   label = paste0("Observed Census:\n", round(observations$obs[[NROW(observations)]]), " on ", format(observations$date[[NROW(observations)]], "%b %d")),
  #   hjust = 0.5,
  #   vjust = 1,
  #   color = "gray23",
  #   fill  = "#F0F0F0",
  #   size = 4.75,
  #   fontface = "bold"
  # ) +
  # ggplot2::annotate(
  #   "segment",
  #   x = observations$date[[NROW(observations)]],
  #   y = observations$obs[[NROW(observations)]],
  #   xend = observations$date[[NROW(observations)]],
  #   yend = min(observations$lower.95[(NROW(observations) - 30):NROW(observations)], predictions$lower.95),
  #   color = "gray23"
  # ) +
  # ggplot2::annotate(
  #   "point",
  #   x = observations$date[[NROW(observations)]],
  #   y = observations$obs[[NROW(observations)]],
  #   color = "gray23"
  # ) +
  # ggplot2::theme(
  #   plot.title = ggplot2::element_text(hjust = 0.5),
  #   # axis.title.y = ggplot2::element_text(size = 18),
  #   legend.title = ggplot2::element_text(size = 20),
  #   legend.text = ggplot2::element_text(size = 20),
  #   legend.position = "bottom",
  #   plot.caption = ggplot2::element_text(hjust = 0.5)
  # ) +
  ggplot2::ylab("COVID+ Census") +
  ggplot2::xlab("Date")
