test_that("dml_plr runs on simulated data", {
  set.seed(1)
  n <- 500
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  D <- 0.5 * x1 - 0.2 * x2 + rnorm(n)
  Y <- 2.0 * D + 0.3 * x1 + 0.1 * x2 + rnorm(n)
  dat <- tibble::tibble(Y, D, x1, x2)

  fit <- dml_plr(
    df = dat,
    y = Y,
    d = D,
    x = c("x1", "x2"),
    m_model = "rf",
    g_model = "rf",
    folds_outer = make_folds(dat, v = 3),
    resamples_tune = rsample::vfold_cv(dat, v = 3),
    grid_size = list(m = 5, g = 5)
  )

  expect_true(is.list(fit))
  expect_true(is.numeric(fit$theta))
  expect_length(fit$ci95, 2)
})
