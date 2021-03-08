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

# Get current infectious activity

current_rt <- rt %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::pull(".pred") %>%
  round(digits = 2) %>%
  as.character()

active <- calc_active_cases(inv_data) %>% format(big.mark = ",")

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
  gt::gtsave(paste0("~/covidModel/figs/rt_table_", Sys.Date(), ".png"))
