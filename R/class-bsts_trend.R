new_bsts_trend <- function(x, class = NULL) {

  if (!is.null(class)) {
    vec_assert(class, ptype = character())
  }

  new_covidmodel_bsts(x, class = c(class, "bsts_trend"))
}

validate_bsts_trend <- function(x, class = NULL) {

  if (!is.null(class)) {
    vec_assert(class, ptype = character())
  }

  new_covidmodel_bsts(x, class = c(class, "bsts_trend"))
}

as_bsts_trend <- function(x, class = NULL) {

  if (!is.null(class)) {
    vec_assert(class, ptype = character())
  }

  as_covidmodel_bsts(x, class = c(class, "bsts_trend"))
}

new_bsts_trend_semilocal <- function(x) {
  new_bsts_trend(x, class = "semilocal")
}

new_bsts_trend_local <- function(x) {
  new_bsts_trend(x, class = "local")
}

new_bsts_trend_robust <- function(x) {
  new_bsts_trend(x, class = "robust")
}

new_bsts_trend_level <- function(x) {
  new_bsts_trend(x, class = "level")
}

validate_bsts_trend_semilocal <- function(x) {
  validate_bsts_trend(x, class = "semilocal")
}

validate_bsts_trend_local <- function(x) {
  validate_bsts_trend(x, class = "local")
}

validate_bsts_trend_robust <- function(x) {
  validate_bsts_trend(x, class = "robust")
}

validate_bsts_trend_level <- function(x) {
  validate_bsts_trend(x, class = "level")
}

as_bsts_trend_semilocal <- function(x) {
  as_bsts_trend(x, class = "semilocal")
}

as_bsts_trend_local <- function(x) {
  as_bsts_trend(x, class = "local")
}

as_bsts_trend_robust <- function(x) {
  as_bsts_trend(x, class = "robust")
}

as_bsts_trend_level <- function(x) {
  as_bsts_trend(x, class = "level")
}
