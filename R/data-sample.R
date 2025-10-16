#' Sample ggplot2::diamonds with a treatment column
#'
#' @param n Number of rows to sample (default 10,000).
#' @param seed RNG seed for reproducibility.
#' @return A tibble with columns from \code{ggplot2::diamonds} plus \code{D} (factor \{0,1\}).
#' @export
diamonds_sample <- function(n = 10000, seed = 1) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "ggplot2 is required to sample diamonds. Please install ggplot2.",
      call. = FALSE
    )
  }

  set.seed(seed)
  result <- ggplot2::diamonds |>
    dplyr::mutate(
      is_rated_ideal = factor(cut == "Ideal", levels = c(FALSE, TRUE))
    ) |>
    dplyr::slice_sample(n = n)

  return(result)
}


#' Precomputed DoubleML replication results (internal)
#' @return A tibble/data.frame with columns: method, seed, theta, se, lwr, upr
#' @export
replication_results <- function() {
  get("dml401k_replications", envir = asNamespace("tiDML"))
}
