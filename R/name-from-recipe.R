#' @keywords internal
recipe_outcome_name <- function(rec, label = "recipe") {
  nm <- summary(rec) |>
    dplyr::filter(role == "outcome") |>
    dplyr::pull(variable)
  if (length(nm) != 1L) {
    stop(label, " must have exactly one outcome.", call. = FALSE)
  }
  nm
}
