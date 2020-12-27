#' Bayesian Structural Time Series: Fit Model
#'
#' Fit a bsts model.
#'
#' @param state A list specifying the state sub-models
#'
#' @param .data The data to use for estimation
#'
#' @param iterations The number of MCMC iterations to perform
#'
#' @param ... Futher arguments to pass to
#'   \code{\link[bsts:bsts]{bsts()}}
#'
#' @export
bsts_fit <- function(
  state,
  .data = state[[".data"]],
  iterations = 1e4,
  ...
) {

  state.spec <- state %>%
    # validate_covidmodel_bsts() %>%
    set_class("list") %>%
    inset2(".data", NULL)

  rlang::inform("")

  bsts::bsts(
    state.specification = state.spec,
    formula = .data,
    niter = iterations,
    ...
  )
}

#' Extract Likelihood of Each MCMC Iteration from `bsts` Object
#'
#' @param .bsts A fitted `bsts` model
#'
#' @param burn The percentage of starting iterations to exclude ("burn")
#'
#' @return A `double` vector of likelihoods
#'
#' @noRd
extract_likelihood_bsts <- function(.bsts, burn = 0.2) {
  .bsts[["log.likelihood"]] %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::slice_tail(prop = 1 - burn) %>%
    dplyr::pull(1L) %>%
    exp()
}

#' Extract Average Prediction Error from `bsts` Object
#'
#' `extract_error_bsts()` calculates the average in-sample, one-step prediction
#' error from a `bsts` object.
#'
#' @inheritParams extract_likelihood_bsts
#'
#' @param likelihood A vector containing the likelihood of each MCMC iteration.
#'   If `NULL`, this will be calculated.
#'
#' @return The prediction error as a scalar `double`
#'
#' @noRd
extract_error_bsts <- function(.bsts, burn = 0.2, likelihood = NULL) {

  if (rlang::is_null(likelihood)) {
    likelihood <- extract_likelihood_bsts(.bsts, burn = burn)
  }

  .bsts[["one.step.prediction.errors"]] %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::slice_tail(prop = 1 - burn) %>%
    abs() %>%
    dplyr::summarize(
      dplyr::across(.fns = ~ weighted.mean(.x, w = likelihood))
    ) %>%
    as.matrix() %>%
    mean()
}
