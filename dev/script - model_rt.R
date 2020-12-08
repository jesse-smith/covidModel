import::from(magrittr, `%>%`)
library(ggplot2)

data <- dplyr::semi_join(
  coviData::load_report_date() %>% dplyr::as_tibble(),
  coviData::process_positive_people(Sys.Date() - 1),
  by = "inv_local_id"
)

last_complete <- estimate_delay(data, rtn = "last_complete") %>%
  dplyr::pull(collection_date)

data %>%
  # Check for logical consistency and valid date ranges
  tidylog::filter(
    collection_date >= "2020-03-05",
    collection_date <= Sys.Date() - 1L,
    report_date >= "2020-04-12",
    report_date <= Sys.Date() - 1L,
    report_date >= collection_date,
    !is.na(collection_date),
    !is.na(report_date)
  ) %>%
  # Start at first contiguous time point
  timetk::filter_by_time(
    .date_var = collection_date,
    .start_date = "2020-03-12",
    .end_date = last_complete
  ) %>%
  dplyr::count(collection_date) %>%
  dplyr::mutate(n = log1p(n)) %>%
  decompose_time_bsts(.col = "n")
  dplyr::transmute(.t = collection_date, n = trend %>% expm1() %>% pmax(0)) %>%
  estimate_rt(.col = "n", .t = ".t", period = 3L) %>%
  dplyr::filter(.t >= "2020-04-01")
rt

library(ggplot2)

ggplot(rt, aes(x = .t)) +
  ggthemes::theme_fivethirtyeight() +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper), alpha = 1/3) +
  geom_point(aes(y = .pred)) +
  geom_line(aes(y = .pred)) +
  coord_cartesian(ylim = c(0, 2)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B")

