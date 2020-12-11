import::from(magrittr, `%>%`)
library(ggplot2)

inv_data <- coviData::process_positive_people(Sys.Date() - 1)

data <- dplyr::semi_join(
  coviData::load_report_date() %>% dplyr::as_tibble(),
  inv_data,
  by = "inv_local_id"
)

last_complete <- estimate_delay(data)

data %>%
  prep_linelist(trend = 30L) %>%
  estimate_rt() ->
rt

data %>%
  prep_linelist(trend = 1L) %>%
  estimate_rt(period = 1L) ->
raw_rt

rt_plot <- plot_rt(rt, raw = raw_rt)

rt_plot

save_plot(
  rt_plot,
  "rt_plot.png"
)
