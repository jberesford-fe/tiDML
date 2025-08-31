#' @export
print.tidyDML_plr <- function(x, ...) {
  cat("DML-PLR estimate (res_y ~ res_d)\n")
  cat(sprintf(
    "  theta: %.6f\n  se:    %.6f\n  95%% CI: [%.6f, %.6f]\n",
    x$theta,
    x$se,
    x$ci95[1],
    x$ci95[2]
  ))
  invisible(x)
}
