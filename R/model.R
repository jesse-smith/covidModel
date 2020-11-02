model_hospital <- function(
  census,
  dates,
  niter = 1100,
  ...
) {
  z <- census %>% log1p() %>% zoo::zoo()

  zoo::index(z) <- dates

  bsts::AddSemilocalLinearTrend(
    y = z,
    slope.ar1.prior = Boom::Ar1CoefficientPrior(
      mu = 1,
      force.stationary = FALSE,
      force.positive = TRUE
    )
  ) %>%
    bsts::bsts(formula = z, niter = niter) ->
  model

  attr(model, which = "trans") <- log1p
  attr(model, which = "inv_trans") <- expm1

  model
}

model_tests <- function(
  tests,
  dates,
  niter = 1100
) {

  y_t <- sqrt(tests)

  z <- if (tibble::is_tibble(y_t)) zoo::zoo(y_t[[1]]) else zoo::zoo(y_t)

  zoo::index(z) <- dates

  bsts::AddSemilocalLinearTrend(
    y = z,
    slope.ar1.prior = Boom::Ar1CoefficientPrior(
      force.stationary = TRUE,
      force.positive   = TRUE
    )
  ) %>%
    bsts::AddSeasonal(
      y = z,
      nseasons = 7
    ) %>%
    bsts::AddHierarchicalRegressionHoliday(
      y = z,
      holiday.list = create_holidays(days.before = 3, days.after = 3)
    ) %>%
    bsts::bsts(formula = z, niter = niter) ->
    model

  attr(model, which = "trans") <- sqrt
  attr(model, which = "inv_trans") <- function(x) x^2

  model
}

model_cases <- function(
  cases,
  dates,
  detected = 1,
  niter = 1100
) {

  trans <- function(x) {
    x %>%
      sqrt() %>%
      culogit(max = sqrt(9.38e5))
  }

  inv_trans <- function(x) {
    x %>%
      cuexpit(max = sqrt(9.38e5)) %>%
      {.^2}
  }

  y_t <- trans(cases)

  z <- if (tibble::is_tibble(y_t)) zoo::zoo(y_t[[1]]) else zoo::zoo(y_t)

  zoo::index(z) <- dates

  bsts::AddSemilocalLinearTrend(
    y = z,
    slope.ar1.prior = Boom::Ar1CoefficientPrior(
      force.stationary = TRUE,
      force.positive   = TRUE
    )
  ) %>%
    bsts::AddSeasonal(
      y = z,
      nseasons = 7
    ) %>%
    bsts::AddHierarchicalRegressionHoliday(
      y = z,
      holiday.list = create_holidays(days.before = 3, days.after = 3)
    ) %>%
    bsts::bsts(formula = z, niter = niter) ->
    model

  l <- attr(y_t, which = "lambda")

  attr(model, which = "trans") <- trans
  attr(model, which = "inv_trans") <- inv_trans

  model
}
