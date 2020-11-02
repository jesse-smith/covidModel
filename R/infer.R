#' @export
infer_trend <- function(x, dates, family = "gaussian", C = 100) {

  z <- zoo::zoo(x) %>% logCp(C = C)

  zoo::index(z) <- dates

  create_model(z) %>%
    bsts::bsts(formula = z, family = family, niter = 1100, ping = 100) ->
    tmod

  state <- tmod$state.contributions
  lik   <- tmod$log.likelihood %>% exp() %>% {. + .Machine$double.eps}

  state %>%
    {.[101:NROW(.), 1, ]} %>%
    apply(MARGIN = 2, FUN = weighted.mean, w = lik[101:NROW(lik)]) %>%
    expmC(C = C) %>%
    zero()
}
