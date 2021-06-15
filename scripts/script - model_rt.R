devtools::load_all()

# Load yesterday's cumulative case data
inv_data <- coviData::process_positive_people()

date <- coviData::path_inv() %>%
  fs::path_file() %>%
  fs::path_ext_remove() %>%
  stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
  lubridate::as_date()

# Combine with the current report date data
data <- dplyr::semi_join(
  coviData::load_report_date() %>% dplyr::as_tibble(),
  inv_data,
  by = "inv_local_id"
) %>%
  dplyr::mutate(collection_date = lubridate::as_date(collection_date))

# Estimate Rt with smoothing and boosting
rt <- estimate_rt(data)
rough_rt <- estimate_rt(data, trend = 1L, boost = FALSE)
rt_plot <- plot_rt(rt, .rough_rt = rough_rt) %T>% {if (interactive()) show(.)}
rt_path <- coviData::path_create(
  "C:/Users/Jesse.Smith/Documents/covidModel/figs/",
  paste0("rt_plot", date),
  ext = "png"
)

covidReport::save_plot(rt_plot, path = rt_path, force = TRUE)

# Get current infectious activity

current_rt <- rt %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::pull(".pred") %>%
  round(digits = 2) %>%
  as.character()

active <- covidReport:::case_calc_active(inv_data) %>%
  dplyr::filter(status == "Active") %>%
  dplyr::pull("n")
rt_tbl_val <- simulate_infections(rt, h = 30) %>%
  vctrs::vec_slice(i = seq(vctrs::vec_size(.) - 29, vctrs::vec_size(.), 1)) %>%
  cumsum() %>%
  vctrs::vec_slice(i = vctrs::vec_size(.) - c(19, 9, 0)) %>%
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
