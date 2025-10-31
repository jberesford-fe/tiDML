#' @keywords internal
assert_cols <- function(data, cols) {
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    stop(
      "These columns are missing from `data`: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

#' @keywords internal
is_valid_treatment <- function(x) {
  if (is.factor(x)) {
    return(length(levels(x)) == 2L)
  }
  if (is.numeric(x) || is.integer(x)) {
    unique_vals <- unique(x[!is.na(x)])
    return(length(unique_vals) > 1L && stats::var(x, na.rm = TRUE) > 0)
  }
  FALSE
}

#' @keywords internal
get_treatment_type <- function(x) {
  if (is.factor(x)) {
    if (length(levels(x)) == 2L) {
      return("binary_factor")
    }
    stop(
      "Factor treatments must have exactly 2 levels, got ",
      length(levels(x)),
      call. = FALSE
    )
  }
  if (is.numeric(x) || is.integer(x)) {
    return("continuous")
  }
  stop("Treatment must be either a binary factor or numeric", call. = FALSE)
}

#' @keywords internal
check_same_data <- function(recipe, data) {
  rec_env <- environment(recipe$template)
  rec_data <- rec_env$data

  if (
    !identical(dim(rec_data), dim(data)) ||
      !identical(names(rec_data), names(data))
  ) {
    stop("The recipe was built on a different dataset than the one supplied.")
  }
  TRUE
}

#' @keywords internal
#' check that the x vars are the same in both recipes
check_same_predictors <- function(rec_g, rec_m) {
  x1 <- rec_g$var_info |>
    dplyr::select(variable, role) |>
    dplyr::filter(role=="predictor")

  x2 <- rec_m$var_info |>
    dplyr::select(variable, role) |>
    dplyr::filter(role=="predictor")

  if (!identical(x1,x2)) {
    stop("The predictor variables in the treatment and outcome recipes do not match.", call. = FALSE)
  }
  TRUE
}


#' @keywords internal
#' check that both workflows use recipes
check_workflows_use_recipes <- function
if (!inherits(m_rec, "recipe") || !inherits(g_rec, "recipe")) {
    stop("Both workflows must use recipes as preprocessors.", call. = FALSE)
  }
