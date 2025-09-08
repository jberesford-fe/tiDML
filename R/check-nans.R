#' @keywords internal
#' Check for NaNs in required columns y and d
check_na_y_d <- function(data, y, d) {
  cols <- c(y, d)
  bad <- purrr::map_lgl(data[cols], anyNA)

  if (any(bad)) {
    stop(
      "Missing values in required columns (y/d): ",
      paste(names(bad)[bad], collapse = ", "),
      ". Please handle NAs before calling dml_plr()."
    )
  }
  invisible(TRUE)
}

#' @keywords internal
check_na_x <- function(data, x) {
  bad_x <- vapply(data[x], anyNA, logical(1))
  if (any(bad_x)) {
    stop(
      "Missing values in predictors: ",
      paste(names(bad_x)[bad_x], collapse = ", "),
      ". Set impute_predictors = TRUE or handle NAs yourself."
    )
  }
}

#' @keywords internal
add_imputation_steps <- function(factory, impute_predictors) {
  function(data, outcome, predictors) {
    rec <- factory(data, outcome, predictors)
    if (impute_predictors) {
      rec <- rec |>
        recipes::step_impute_median(recipes::all_numeric_predictors()) |>
        recipes::step_impute_mode(recipes::all_nominal_predictors())
    }
    rec
  }
}
