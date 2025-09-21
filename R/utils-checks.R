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
is_binary_like <- function(x) {
  if (is.factor(x)) {
    return(length(levels(x)) == 2L)
  }
  if (is.numeric(x) || is.integer(x) || is.logical(x)) {
    ux <- sort(unique(x))
    return(all(ux %in% c(0, 1)) && length(ux) <= 2L)
  }
  FALSE
}

#' @keywords internal
treated_level <- function(d_vec) {
  if (is.factor(d_vec)) {
    lv <- levels(d_vec)
    if (length(lv) != 2L) {
      stop("`d` must be binary (two levels) for classification.", call. = FALSE)
    }
    # Convention: second level is "treated"
    return(lv[2])
  }
  # Numeric/logical -> treated is 1
  1
}
