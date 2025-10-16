#' @keywords internal
assert_cols <- function(data, cols) {
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    stop(
      "These columns are missing from `data`: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

#' @keywords internal
is_valid_treatment <- function(x) {
  if (is.factor(x)) {
    return(length(levels(x)) == 2L)
  }
  if (is.numeric(x) || is.integer(x)) {
    unique_vals <- unique(x[!is.na(x)])
    return(length(unique_vals) > 1L && stats::var(x, na.rm = TRUE) > 0)
  }
  FALSE
}

#' @keywords internal
get_treatment_type <- function(x) {
  if (is.factor(x)) {
    if (length(levels(x)) == 2L) {
      return("binary_factor")
    }
    stop(
      "Factor treatments must have exactly 2 levels, got ",
      length(levels(x)),
      call. = FALSE
    )
  }
  if (is.numeric(x) || is.integer(x)) {
    return("continuous")
  }
  stop("Treatment must be either a binary factor or numeric", call. = FALSE)
}
