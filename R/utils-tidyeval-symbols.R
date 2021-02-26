#' Symbols Used in Tidy Evaluation
#'
#' Import tidy eval pronouns (\code{\link[rlang:tidyeval-data]{.data}} and
#' \code{\link[rlang:tidyeval-data]{.env}}), as well as forcing operators
#' (\code{\link[rlang:nse-force]{!!}}, \code{\link[rlang:nse-force]{!!!}}, and
#' \code{\link[rlang:nse-force]{:=}}).
#' These operators cannot be called using the usual `::` syntax and must thus
#' be imported to prevent R CMD CHECK warnings.
#'
#' @name utils-tidyeval-symbols
#'
#' @keywords internal
#'
#' @importFrom rlang .data .env !! !!! :=
NULL
