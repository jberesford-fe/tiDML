test_that("dml_rf runs on simulated data and recovers the effect", {
  set.seed(1)
  n <- 800
  x1 <- rnorm(n)
  x2 <- rnorm(n)

  # binary treatment with nonlinear propensity
  p <- plogis(0.5 * x1 - 0.2 * x2)
  Db <- rbinom(n, 1, p)
  d <- factor(Db, levels = c(0, 1)) # factor → classification path

  # outcome with true theta = 2.0
  y <- 2.0 * Db + 0.3 * x1 + 0.1 * x2 + rnorm(n)

  dat <- tibble::tibble(y = y, d = d, x1 = x1, x2 = x2)

  # same stratified folds
  folds <- make_folds_stratified(dat, d = "d", n_folds = 3)

  fit <- dml_rf(
    data = dat,
    y = "y",
    d = "d",
    x = c("x1", "x2"),
    folds_outer = folds,
    vcov_type = "HC3"
  )

  # object basics
  expect_s3_class(fit, "dml_plr")
  expect_type(fit$estimate, "double")
  expect_true(is.numeric(fit$se))
  expect_length(fit$ci_95, 2)

  # close to true theta (allow some noise)
  expect_equal(unname(fit$estimate), 2.0, tolerance = 0.3)

  # generics methods
  tt <- generics::tidy(fit)
  expect_s3_class(tt, "tbl_df")
  expect_true(all(
    c("estimate", "std.error", "conf.low", "conf.high") %in% names(tt)
  ))

  aug <- augment(fit)
  expect_s3_class(aug, "tbl_df")
  expect_equal(nrow(aug), n)
  expect_true(all(
    c("y", "d", "g_hat", "m_hat", "y_res", "d_res") %in% names(aug)
  ))
})
