#' @export
create_model <- function(x, dates = NULL, niter = 1100, holiday_window = c(3,3)) {
  if (zoo::is.zoo(x)) {
    z <- x
  } else {
    assertthat::assert_that(
      !is.null(dates),
      msg = "'dates' must be provided if 'x' is not a zoo object"
    )
    z <- zoo::zoo(x)
    zoo::index(z) <- dates
  }

  bsts::AddLocalLinearTrend(y = z) %>%
    bsts::AddSeasonal(y = z, nseasons = 7) %>%
    bsts::AddHierarchicalRegressionHoliday(
      holiday.list = create_holidays(days.before = holiday_window[[1]], days.after = holiday_window[[2]]),
      y = z
    ) %>%
    bsts::bsts(formula = z, niter = niter)
}

#' @export
create_holidays <- function(holiday_names = "Default", days.before = 3, days.after = 3) {
  holidays <- list()

  if (holiday_names == "Default") {
    holiday_names <- c(
      "NewYearsDay",
      "SuperBowlSunday",
      "MartinLutherKingDay",
      "ValentinesDay",
      "SaintPatricksDay",
      "EasterSunday",
      "USMothersDay",
      "IndependenceDay",
      "LaborDay",
      "Halloween",
      "Thanksgiving",
      "MemorialDay",
      "VeteransDay",
      "Christmas"
    )
  }

  for (holiday in holiday_names) {
    holidays <- append(
      holidays,
      list(
        bsts::NamedHoliday(
          holiday.name = holiday,
          days.before = days.before,
          days.after = days.after
        )
      )
    )
  }
  holidays
}
