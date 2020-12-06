data <- process_daily_data()

latest_complete <- estimate_delay(data)$.t

data %>%
  summarize_incidence() %>%
  filter(.t <= latest_complete) %>%
  estimate_rt() %>%
  filter(.t >= "2020-04-01")