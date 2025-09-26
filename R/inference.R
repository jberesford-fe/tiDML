#' @keywords internal
plr_estimate <- function(y_res, d_res, vcov_type = "HC2") {
  dat <- data.frame(y_res = y_res, d_res = d_res)
  fit <- stats::lm(y_res ~ 0 + d_res, data = dat)
  V <- sandwich::vcovHC(fit, type = vcov_type)
  co <- stats::coef(fit)[[1]]
  se <- sqrt(diag(V))[[1]]
  ci <- co + c(-1, 1) * stats::qnorm(0.975) * se
  list(theta = co, se = se, ci = ci, lm_fit = fit, vcov = V)
}
