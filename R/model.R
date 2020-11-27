model_hospital <- function(
  census,
  dates,
  niter = 1100
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
