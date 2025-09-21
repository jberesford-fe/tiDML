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
  if (!is_binary_like(data[[d]])) {
    warning("`d` is not binary-like; returning unstratified folds.")
    return(rsample::vfold_cv(data, v = n_folds))
  }
  rsample::vfold_cv(data, v = n_folds, strata = !!rlang::sym(d))
}
