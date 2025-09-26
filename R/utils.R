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
#' @return rsample rset
#' @export
make_folds_stratified <- function(data, d, n_folds = 5) {
  d <- rlang::as_name(rlang::ensym(d))
  treatment_type <- get_treatment_type(data[[d]])

  if (treatment_type == "binary_factor") {
    # Stratify on factor levels
    rsample::vfold_cv(data, v = n_folds, strata = !!rlang::sym(d))
  } else {
    # Continuous treatment: no stratification needed
    message("Continuous treatment detected: using unstratified folds.")
    rsample::vfold_cv(data, v = n_folds)
  }
}

oob_error <- function(fitted_wf) {
  mod <- workflows::extract_fit_parsnip(fitted_wf)$fit
  pe <- mod$prediction.error

  return(pe)
}
