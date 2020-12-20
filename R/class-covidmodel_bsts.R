new_covidmodel_bsts <- function(x, class = NULL) {

  if (!is.null(class)) {
    vec_assert(class, ptype = character())
  }

  classes <- c(class, "covidmodel_bsts", "list")

  set_class(x, classes)
}


validate_covidmodel_bsts <- function(x, class = NULL) {

  if (!is.null(class)) {
    vec_assert(class, ptype = character())
  }

  is_covidmodel_bsts <- x %>%
    class() %>%
    vec_slice(i = c(vec_size(.) - 1L, vec_size(.))) %>%
    paste0(collapse = " ") %>%
    equals("covidmodel_bsts list")

  if (!is_covidmodel_bsts) {
    rlang::abort("Object is not a list of class `covidmodel_bsts`")
  }

  if (!is.null(class)) {
    is_class <- x %>%
      class() %>%
      vec_slice(i = vec_seq_along(class)) %>%
      paste0(collapse = " ") %>%
      equals(paste0(class, collapse = " "))

    if (!is_class) {
      rlang::abort(
        paste0("Object is not a `covidmodel_bsts` list of class `", class, "`")
      )
    }
  }

  x
}

as_covidmodel_bsts <- function(x, class = NULL) {
  x %>%
    rlang::as_list() %>%
    new_covidmodel_bsts(class = class) %>%
    validate_covidmodel_bsts(class = class)
}
