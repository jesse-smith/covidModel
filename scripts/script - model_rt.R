devtools::load_all()

# Load yesterday's cumulative case data
inv_data <- coviData::process_positive_people()

# Combine with the current report date data
data <- dplyr::semi_join(
  coviData::load_report_date() %>% dplyr::as_tibble(),
  inv_data,
  by = "inv_local_id"
) %>%
  dplyr::mutate(collection_date = lubridate::as_date(collection_date))

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

current_rt <- rt %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::pull(".pred") %>%
  round(digits = 2) %>%
  as.character()

# c_inf <- simulate_infections(rt, h = 365) %>%
#   vec_slice(i = (vec_size(.)-364):vec_size(.)) %>%
#   cumsum()
#
# dt <- vec_size(c_inf[c_inf <= 76534])
#
# paste0("Doubling Time: ", dt, " days")

active <- 2911 %>% format(big.mark = ",")

rt_tbl_val <- simulate_infections(rt, h = 30) %>%
  vec_slice(i = seq(vec_size(.) - 29, vec_size(.), 1)) %>%
  cumsum() %>%
  vec_slice(i = vec_size(.) - c(19, 9, 0)) %>%
  round() %>%
  format(big.mark = ",") %>%
  stringr::str_squish() %>%
  {paste0("**", ., "**")} %>%
  purrr::prepend(c(current_rt, active))

rt_tbl_nm <- c(
  "Rt",
  "Active Cases",
  "**Cases over next 10 Days**",
  "**Cases over next 20 Days**",
  "**Cases over next 30 Days**"
)
rt_tbl <- tibble::tibble(`Cases` = rt_tbl_nm, Count = rt_tbl_val)

title <- paste0("Implications of Current Rt and Active Cases")
gt::gt(rt_tbl) %>%
  gt::tab_header(title = title) %>%
  gt::opt_row_striping() %>%
  gt::cols_label(Cases = "", Count = "") %>%
  gt::fmt_markdown(columns = dplyr::everything(), rows = c(F, F, T,T,T)) %T>%
  {show(.)} %>%
  gt::gtsave(paste0("figs/rt_table_", Sys.Date(), ".png"))
