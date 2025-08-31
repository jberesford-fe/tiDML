#' Default recipe blueprint for nuisance models
#' @param data Training data
#' @param outcome Outcome column (symbol or string)
#' @param predictors Character vector of predictor names
#' @return a recipes::recipe
#' @export
base_recipe <- function(data, outcome, predictors) {
  outcome <- rlang::ensym(outcome)
  form <- stats::as.formula(paste(rlang::as_name(outcome), "~ ."))
  df <- dplyr::select(
    data,
    dplyr::all_of(c(rlang::as_name(outcome), predictors))
  )
  recipes::recipe(form, data = df) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    recipes::step_dummy(recipes::all_nominal_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())
}

#' Resolve a recipe input into a factory function
#' @param recipe_in NULL, a recipe object, or a factory function(data, outcome, predictors)->recipe
#' @return function(data, outcome, predictors) -> recipe
#' @keywords internal
#' @export
resolve_recipe_factory <- function(recipe_in = NULL) {
  if (is.null(recipe_in)) {
    return(base_recipe)
  }
  if (inherits(recipe_in, "recipe")) {
    rec_obj <- recipe_in
    return(function(data, outcome, predictors) {
      if (is.null(rec_obj$term_info) || nrow(rec_obj$term_info) == 0) {
        outcome <- rlang::ensym(outcome)
        form <- stats::as.formula(paste(rlang::as_name(outcome), "~ ."))
        rec_obj <<- recipes::update_recipe(rec_obj, formula = form)
      }
      rec_obj
    })
  }
  if (is.function(recipe_in)) {
    return(recipe_in)
  }
  stop(
    "`recipe_*` must be NULL, a recipes::recipe, or a function(data, outcome, predictors) -> recipe."
  )
}
