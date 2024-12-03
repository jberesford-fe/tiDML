#' Example Dataset for tiDML
#'
#' A simulated dataset for demonstrating Double Machine Learning (DML) methods.
#'
#' @format A dataframe with 500 rows and 4 variables:
#' \describe{
#'   \item{Y}{Outcome variable (numeric).}
#'   \item{D}{Treatment variable (numeric).}
#'   \item{X1}{Confounder 1 (numeric).}
#'   \item{X2}{Confounder 2 (numeric).}
#' }
#' @source Simulated data
#' @examples
#' data <- tiDML::example_data()
#' summary(data)
"example_data"
