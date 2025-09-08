#' @param data A data.frame/tibble
#' @param v Number of folds
#' @param strata Optional column to stratify by
#' @return caret folds indices
#' @export
make_folds <- function(data, v = 5, strata = NULL) {
  if (!is.null(strata)) {
    caret::createFolds(data[[strata]], k = v, returnTrain = FALSE)
  } else {
    caret::createFolds(seq_len(nrow(data)), k = v, returnTrain = FALSE)
  }
}
