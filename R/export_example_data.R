#' Load Example Data
#'
#' This function loads example data for demonstrating the tiDML package.
#'
#' @return A dataframe with simulated data for DML.
#' @examples
#' df <- tiDML::example_data()
#' head(df)
#' @export
example_data <- function() {
  data("example_data", package = "tiDML")
  example_data
}
