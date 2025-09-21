#' Make v-fold outer folds
#' @param data Data frame
#' @param v Number of folds
#' @return rsample rset
#' @export
make_folds <- function(data, v = 5) {
  rsample::vfold_cv(data, v = v)
}

#' Make stratified v-fold outer folds (recommended for binary D)
#' @param data Data frame
#' @param d Treatment column name (string)
#' @param v Number of folds
#' @return rsample rset
#' @export
make_folds_stratified <- function(data, d, v = 5) {
  d <- rlang::as_name(rlang::ensym(d))
  if (!is_binary_like(data[[d]])) {
    warning("`d` is not binary-like; returning unstratified folds.")
    return(rsample::vfold_cv(data, v = v))
  }
  rsample::vfold_cv(data, v = v, strata = !!rlang::sym(d))
}
