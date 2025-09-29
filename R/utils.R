#' Make v-fold outer folds
#' @param data Data frame
#' @param n_folds Number of folds
#' @return rsample rset
#' @export
make_folds <- function(data, n_folds = 5) {
  rsample::vfold_cv(data, v = n_folds)
}

#' Make stratified v-fold outer folds (recommended for binary D)
#' @param data Data frame
#' @param d Treatment column name (string)
#' @param n_folds Number of folds
#' @param n_rep Number of repeats
#' @return rsample rset
#' @export
make_folds_stratified <- function(data, d, n_folds = 5, n_rep = 1) {
  d_name <- if (is.character(d)) {
    d
  } else {
    rlang::as_name(rlang::ensym(d))
  }

  treatment_type <- get_treatment_type(data[[d_name]])

  if (treatment_type == "binary_factor") {
    rsample::vfold_cv(
      data,
      v = n_folds,
      repeats = n_rep,
      strata = !!rlang::sym(d_name)
    )
  } else {
    message("Continuous treatment detected: using unstratified folds.")
    rsample::vfold_cv(data, v = n_folds, repeats = n_rep)
  }
}

#' Get out of bad error
#' @param d_vec Treatment vector
#' @return Treatment type string
#' @keywords internal
oob_error <- function(fitted_wf) {
  mod <- workflows::extract_fit_parsnip(fitted_wf)$fit
  pe <- mod$prediction.error

  return(pe)
}

#' Get the "treated" level of a binary factor
#' @param d_vec Binary factor vector
#' @return The "treated" level (second level)
#' @keywords internal
treated_level <- function(d_vec) {
  if (!is.factor(d_vec) || length(levels(d_vec)) != 2L) {
    stop("`d` must be binary factor for treated_level()", call. = FALSE)
  }
  # Convention: second level is "treated"
  levels(d_vec)[2]
}
