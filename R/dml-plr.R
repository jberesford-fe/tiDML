#' DML-PLR (Partially Linear Regression) with tidymodels
#'
#' @param df Data frame
#' @param y Outcome column
#' @param d Treatment column
#' @param x Character vector of predictors
#' @param folds_outer Outer folds (shared by both nuisances) for cross-fitting
#' @param resamples_tune Resamples for global tuning (e.g., vfold_cv on df)
#' @param m_model,g_model Either a parsnip spec or one of "rf","xgb","glmnet","linear"
#' @param recipe_shared Optional recipe or factory used for both nuisances
#' @param recipe_m,recipe_g Optional separate recipe or factory for each nuisance
#' @param grid_size list(m=, g=) grid sizes for tuning
#' @param vcov_type Sandwich variance type (e.g., "HC2")
#' @return list with estimate, SE, CI, and objects for audit
#' @export
dml_plr <- function(
  df,
  y,
  d,
  x,
  folds_outer = make_folds(df, v = 5),
  resamples_tune = rsample::vfold_cv(df, v = 5),
  m_model = "rf",
  g_model = "rf",
  recipe_shared = NULL,
  recipe_m = NULL,
  recipe_g = NULL,
  grid_size = list(m = 15, g = 15),
  vcov_type = "HC2"
) {
  y_sym <- rlang::ensym(y)
  d_sym <- rlang::ensym(d)
  y_name <- rlang::as_name(y_sym)
  d_name <- rlang::as_name(d_sym)
  x <- vapply(x, rlang::as_name, character(1))

  df <- do.call(
    tidyr::drop_na,
    c(list(df), as.list(unname(c(y_name, d_name, x))))
  )

  if (!(".row_id" %in% names(df))) {
    df$.row_id <- seq_len(nrow(df))
  }

  # recipes
  shared_factory <- resolve_recipe_factory(recipe_shared)

  m_factory <- resolve_recipe_factory(
    if (!is.null(recipe_m)) recipe_m else shared_factory
  )

  g_factory <- resolve_recipe_factory(
    if (!is.null(recipe_g)) recipe_g else shared_factory
  )

  # specs (force regression for PLR)
  m_spec <- resolve_spec(m_model, mode = "regression")
  g_spec <- resolve_spec(g_model, mode = "regression")
  m_spec <- ensure_mode(m_spec, df[[rlang::as_name(d)]])
  g_spec <- ensure_mode(g_spec, df[[rlang::as_name(y)]])

  if (m_spec$mode != "regression" || g_spec$mode != "regression") {
    stop("PLR requires regression specs for both m(X): D~X and g(X): Y~X.")
  }

  # tuning (global; fast)
  rec_m <- m_factory(df, d_name, x)
  rec_g <- g_factory(df, y_name, x)

  tuned_m <- tune_any(
    m_spec,
    rec_m,
    resamples_tune,
    df[, x, drop = FALSE],
    grid_size = grid_size$m
  )
  tuned_g <- tune_any(
    g_spec,
    rec_g,
    resamples_tune,
    df[, x, drop = FALSE],
    grid_size = grid_size$g
  )

  # cross-fitting (shared outer folds)
  res_d <- crossfit_residuals(
    df,
    d_name,
    x,
    tuned_m$final_spec,
    folds_outer,
    m_factory
  )
  res_y <- crossfit_residuals(
    df,
    y_name,
    x,
    tuned_g$final_spec,
    folds_outer,
    g_factory
  )

  # final stage
  fin <- tibble::tibble(res_y = res_y, res_d = res_d) |> tidyr::drop_na()
  ols <- stats::lm(res_y ~ res_d, data = fin)
  V <- sandwich::vcovHC(ols, type = vcov_type)
  ct <- lmtest::coeftest(ols, vcov. = V)

  row <- which(rownames(ct) == "res_d")
  theta <- unname(ct[row, "Estimate"])
  se <- unname(ct[row, "Std. Error"])
  ci95 <- theta + c(-1, 1) * stats::qnorm(0.975) * se

  structure(
    list(
      theta = theta,
      se = se,
      ci95 = ci95,
      final_lm = ols,
      vcov = V,
      coeftest = ct,
      folds_outer = folds_outer,
      resamples_tune = resamples_tune,
      tuned_m = tuned_m,
      tuned_g = tuned_g,
      call = match.call()
    ),
    class = "tidyDML_plr"
  )
}
