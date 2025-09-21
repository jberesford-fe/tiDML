#' Sample ggplot2::diamonds with a treatment column
#'
#' @param n Number of rows to sample (default 10,000).
#' @param seed RNG seed for reproducibility.
#' @return A tibble with columns from `ggplot2::diamonds` plus `D` (factor {0,1}).
#' @export
diamonds_sample <- function(n = 10000, seed = 1) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "ggplot2 is required to sample diamonds. Please install ggplot2.",
      call. = FALSE
    )
  }
  set.seed(seed)
  ggplot2::diamonds |>
    dplyr::mutate(
      is_rated_ideal = factor(ifelse(cut == "Ideal", 1L, 0L), levels = c(0, 1))
    ) |>
    dplyr::slice_sample(n = n)
}
