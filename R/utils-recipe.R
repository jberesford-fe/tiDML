#' Default recipe blueprint for nuisance models
#' @param data Training data
#' @param outcome Outcome column (symbol or string)
#' @param predictors Character vector of predictor names
#' @return a recipes::recipe
#' @export
base_recipe <- function(data, outcome, predictors) {
  # If outcome is a character, this forces evaluation and uses it directly.
  outcome_name <- if (is.character(outcome) && length(outcome) == 1) {
    outcome
  } else {
    rlang::as_name(rlang::ensym(outcome))
  }

  # Same idea for predictors: allow character vector or bare names
  if (is.character(predictors)) {
    predictors_chr <- predictors
  } else {
    predictors_chr <- vapply(predictors, rlang::as_name, character(1))
  }

  # (your helpful prints)
  print(paste("base_recipe outcome_name:", outcome_name))
  print(paste(
    "base_recipe predictors_chr:",
    paste(predictors_chr, collapse = ", ")
  ))
  print(paste("base_recipe colnames:", paste(colnames(data), collapse = ", ")))

  missing_cols <- setdiff(c(outcome_name, predictors_chr), colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "Column(s) not found in dataframe: ",
      paste(missing_cols, collapse = ", "),
      ". Columns are: ",
      paste(colnames(data), collapse = ", ")
    ))
  }

  df_sub <- dplyr::select(data, dplyr::all_of(c(outcome_name, predictors_chr)))

  recipes::recipe(
    stats::as.formula(paste(outcome_name, "~ .")),
    data = df_sub
  ) |>
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
