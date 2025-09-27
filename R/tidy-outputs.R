#' @export
#' @importFrom generics tidy
tidy.dml_plr <- function(x, ...) {
  d_name <- attr(x, "d_name")

  # Create term name with treated level for factors
  term_name <- if (!is.null(d_name)) {
    # Check if we have treatment type information
    treatment_type <- x$treatment_type

    if (!is.null(treatment_type) && treatment_type == "binary_factor") {
      # Get the treated level from the original data
      if (!is.null(x$.d_orig) && is.factor(x$.d_orig)) {
        treated_level <- levels(x$.d_orig)[2] # Second level is treated
        paste0(d_name, treated_level)
      } else {
        d_name
      }
    } else {
      d_name # For continuous treatments, just use the name
    }
  } else {
    "treatment"
  }

  tibble::tibble(
    term = term_name,
    estimate = x$estimate,
    std.error = x$se,
    conf.low = x$ci_95[1],
    conf.high = x$ci_95[2],
    vcov_type = x$vcov_type
  )
}

#' @export
#' @importFrom generics augment
augment.dml_plr <- function(x, ...) {
  if (is.null(x$.y_orig) || is.null(x$.d_orig)) {
    stop(
      "augment.dml_plr(): originals (.y_orig/.d_orig) were not stored in the fit.",
      call. = FALSE
    )
  }
  n <- length(x$y_res)
  if (length(x$.y_orig) != n || length(x$.d_orig) != n) {
    stop(
      "augment.dml_plr(): stored originals have different length than residuals.",
      call. = FALSE
    )
  }

  tibble::tibble(
    .row = seq_len(n),
    y = x$.y_orig,
    d = x$.d_orig,
    g_hat = x$g_hat,
    m_hat = x$m_hat,
    y_res = x$y_res,
    d_res = x$d_res,
  )
}
