#' @export
nowcast_cases <- function(
  cases,
  dates,
  family = "gaussian",
  delay = 5,
  now = Sys.Date(),
  C = 100
) {
  z <- zoo::zoo(cases[1:(NROW(cases) - delay)]) %>% logCp(C = C)
  zoo::index(z) <- dates[1:(NROW(dates) - delay)]

  create_model(z) %>%
    bsts::bsts(formula = z, family = family, niter = 1100, ping = 100) %>%
    bsts::predict.bsts(horizon = delay, burn = 100) %>%
    .$mean %>%
    expmC(C = C) %>%
    zero() %>%
    {c(rep(NA, times = NROW(cases) - delay), .)}
}
