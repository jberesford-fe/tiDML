#' @export
#' @importFrom generics tidy
tidy.dml_plr <- function(x, ...) {
  tibble::tibble(
    term = "d_res",
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
