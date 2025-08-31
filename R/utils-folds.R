#' @param data A data.frame/tibble
#' @param v Number of folds
#' @param strata Optional column to stratify by
#' @return rsample vfold_cv object
#' @export
make_folds <- function(data, v = 5, strata = NULL) {
  if (!is.null(strata)) {
    rsample::vfold_cv(data, v = v, strata = !!rlang::ensym(strata))
  } else {
    rsample::vfold_cv(data, v = v)
  }
}
