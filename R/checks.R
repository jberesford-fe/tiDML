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
    # Must be exactly binary for factors
    return(length(levels(x)) == 2L)
  }
  if (is.numeric(x) || is.integer(x)) {
    # For numeric: check for sufficient variation
    unique_vals <- unique(x[!is.na(x)])
    return(length(unique_vals) > 1L && var(x, na.rm = TRUE) > 0)
  }
  FALSE
}


#' @keywords internal
get_treatment_type <- function(x) {
  if (is.factor(x)) {
    if (length(levels(x)) == 2L) {
      return("binary_factor")
    }
    stop("Factor treatments must be binary (exactly 2 levels)", call. = FALSE)
  }
  if (is.numeric(x) || is.integer(x)) {
    return("continuous")
  }
  stop("Treatment must be either a binary factor or numeric", call. = FALSE)
}
