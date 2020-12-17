devtools::load_all()

# Load yesterday's cumulative case data
inv_data <- coviData::process_positive_people(Sys.Date() - 1)

# Combine with the current report date data
data <- dplyr::semi_join(
  coviData::load_report_date() %>% dplyr::as_tibble(),
  inv_data,
  by = "inv_local_id"
)
# Remove inv_data to save space
remove(inv_data)

# Estimate Rt with smoothing and boosting
rt <- estimate_rt(data)

# Unsmoothed Rt
rough_rt <- estimate_rt(data, trend = 1L, boost = FALSE)

# Create and show figure
rt_plot <- plot_rt(rt, .rough_rt = rough_rt) %T>% show()

# Save to figs
save_plot(
  rt_plot,
  paste0("figs/rt_plot", Sys.Date(), ".png")
)

# Get current infectious activity

simulate_infections(rt, h = 365) %>% plot()
  vec_slice(i = seq(vec_size(.) - 49, vec_size(.), 1)) %>%
  cumsum()
