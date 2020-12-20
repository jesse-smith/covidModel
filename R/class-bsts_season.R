#'
new_bsts_season <- function(x, class = NULL) {

  if (!is.null(class)) {
    vec_assert(class, ptype = character())
  }

  new_covidmodel_bsts(x, class = c(class, "bsts_season"))
}

validate_bsts_season <- function(x, class = NULL) {

  if (!is.null(class)) {
    vec_assert(class, ptype = character())
  }

  validate_covidmodel_bsts(x, class = c(class, "bsts_season"))
}

as_bsts_season <- function(x, class = NULL) {

  if (!is.null(class)) {
    vec_assert(class, ptype = character())
  }

  as_covidmodel_bsts(x, class = c(class, "bsts_season"))
}

new_bsts_season_regression <- function(x) {
  new_bsts_season(x, class = "regression")
}

new_bsts_season_harmonic <- function(x) {
  new_bsts_season(x, class = "harmonic")
}

new_bsts_season_dynamic <- function(x) {
  new_bsts_season(x, class = "dynamic")
}

validate_bsts_season_regression <- function(x) {
  validate_bsts_season(x, class = "regression")
}

validate_bsts_season_harmonic <- function(x) {
  validate_bsts_season(x, class = "harmonic")
}

validate_bsts_season_dynamic <- function(x) {
  validate_bsts_season(x, class = "dynamic")
}

as_bsts_season_regression <- function(x) {
  as_bsts_season(x, class = "regression")
}

as_bsts_season_harmonic <- function(x) {
  as_bsts_season(x, class = "harmonic")
}

as_bsts_season_dynamic <- function(x) {
  as_bsts_season(x, class = "dynamic")
}
