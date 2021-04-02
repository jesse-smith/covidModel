bsts_ar1_regression <- function(
  state = list(),
  formula,
  .data = state[[".data"]],
  ...
) {

  state.spec <- state %>%
    set_class("list") %>%
    inset2(".data", NULL)

  formula <- rlang::enexpr(formula) %>%
    rlang::expr_text() %>%
    stats::as.formula()

  bsts::AddDynamicRegression(
    state.specification = state.spec,
    formula = formula,
    data = .data,
    ...
  )
}
