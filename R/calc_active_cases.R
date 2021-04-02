#' Calculate Active Cases Based on 14 Day Heuristic
#'
#' @param data Case data from the investigation file, as output by
#'   \code{\link[coviData:process_positive_people]{process_positive_people()}}
#'
#' @param date The date of the data file to use; if `NULL`, the most recent data
#'   available will be used
#'
#' @param quiet Should info messages be silenced?
#'
#' @return An `integer` number of active cases on `date`
#'
#' @export
calc_active_cases <- function(
  data = coviData::process_positive_people(date = date),
  date = NULL,
  quiet = FALSE
) {
  if (is.null(date)) {
    date <- coviData::path_inv() %>%
      stringr::str_extract("[^/~]+$") %>%
      stringr::str_extract("^[^.]+") %>%
      stringr::str_extract("[0-9]{1,4}[^0-9]*[0-9]{1,2}[^0-9][0-9]{1,2}") %>%
      stringr::str_replace_all("[^0-9]+", "-")
  }

  date <- coviData::std_dates(
    date,
    force = "dt",
    train = FALSE,
    orders = c("ymd", "mdy", "dmy")
  )

  if (is.null(date) && !quiet) rlang::inform(paste("Date used: ", date))

  data %>%
    dplyr::mutate(
      .onset_dt_tmp_ = coviData::std_dates(
        .data[["illness_onset_dt"]],
        force = "dt",
        train = FALSE,
        orders = "YmdT"
      ),
      .collect_dt_tmp_ = coviData::std_dates(
        .data[["specimen_coll_dt"]],
        force = "dt",
        train = FALSE,
        orders = "YmdT"
      ),
      .inv_dt_tmp_ = coviData::std_dates(
        .data[["inv_start_dt"]],
        force = "dt",
        train = FALSE,
        orders = "YmdT"
      ),
      .ref_dt_tmp_ = dplyr::coalesce(
        .data[[".onset_dt_tmp_"]],
        .data[[".collect_dt_tmp_"]],
        .data[[".inv_dt_tmp_"]]
      ),
      .days_from_ref_tmp_ = as.integer(date - .data[[".ref_dt_tmp_"]])
    ) %>%
    dplyr::filter(dplyr::between(.data[[".days_from_ref_tmp_"]], 0L, 14L)) %>%
    vec_size()
}
