import::from(magrittr, `%>%`)
library(ggplot2)

inv_data <- coviData::process_positive_people(Sys.Date() - 1)

data <- dplyr::semi_join(
  coviData::load_report_date() %>% dplyr::as_tibble(),
  inv_data,
  by = "inv_local_id"
)

rt <- rt_from_linelist(data)

jagged_rt <- rt_from_linelist(data, trend = 1L, resample = FALSE)

rt_plot <- plot_rt(rt, raw = jagged_rt)

rt_plot

save_plot(
  rt_plot,
  paste0("figs/rt_plot", Sys.Date(), ".png")
)
