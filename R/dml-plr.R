#' DML-PLR (Partially Linear Regression) with tidymodels
#'
#' @param data Data frame
#' @param y Outcome column
#' @param d Treatment column
#' @param x Character vector of predictors
#' @param folds_outer Outer folds (shared by both nuisances) for cross-fitting
#' @param resamples_tune Resamples for global tuning (e.g., vfold_cv on df)
#' @param m_model Treatment model: either a parsnip spec or one of "rf","xgb","glmnet","linear"
#' @param g_model Outcome model: either a parsnip spec or one of "rf","xgb","glmnet","linear"
#' @param recipe_m,recipe_g Optional separate recipe or factory for each nuisance
#' @param grid_size list(m=, g=) grid sizes for tuning
#' @param vcov_type Sandwich variance type (e.g., "HC2")
#' @return list with estimate, SE, CI, and objects for audit
#' @export
dml_plr <- function(
  data,
  y,
  d,
  x,
  folds_outer = make_folds(data, v = 5),
  resamples_tune = rsample::vfold_cv(data, v = 5),
  m_model = "rf",
  g_model = "rf",
  recipe_m = NULL,
  recipe_g = NULL,
  impute_predictors = FALSE,
  grid_size = list(m = 15, g = 15),
  vcov_type = "HC2"
) {
  # Read strings or symbols as col names
  y_sym <- rlang::ensym(y)
  d_sym <- rlang::ensym(d)
  y_name <- rlang::as_name(y_sym)
  d_name <- rlang::as_name(d_sym)
  x <- vapply(x, rlang::as_name, character(1))

  # Error if y or d have NAs
  check_na_y_d(data, y_name, d_name)

  # recipes
  m_factory0 <- resolve_recipe_factory(recipe_m)
  g_factory0 <- resolve_recipe_factory(recipe_g)

  if (impute_predictors) {
    m_factory <- add_imputation_steps(m_factory0, TRUE)
    g_factory <- add_imputation_steps(g_factory0, TRUE)
  } else {
    check_na_x(data, x)
    m_factory <- m_factory0
    g_factory <- g_factory0
  }

  d_is_factor <- is.factor(data[[d_name]])
  m_mode <- if (d_is_factor) "classification" else "regression"
  g_mode <- "regression"

  if (is.factor(data[[y_name]])) {
    stop("Outcome y must be numeric for dml_plr.")
  }

  message(sprintf("treatment model (first stage) mode set to: %s", m_mode))

  m_spec <- resolve_spec(m_model, mode = m_mode)
  g_spec <- resolve_spec(g_model, mode = g_mode)

  # tuning (global; fast)
  rec_m <- m_factory(data, d_name, x)
  rec_g <- g_factory(data, y_name, x)

  tuned_m <- tune_any(
    m_spec,
    rec_m,
    resamples_tune,
    data[, x, drop = FALSE],
    grid_size = grid_size$m
  )
  tuned_g <- tune_any(
    g_spec,
    rec_g,
    resamples_tune,
    data[, x, drop = FALSE],
    grid_size = grid_size$g
  )

  # cross-fitting (shared outer folds)
  res_d <- crossfit_residuals(
    data,
    d_name,
    x,
    tuned_m$final_spec,
    folds_outer,
    m_factory
  )
  res_y <- crossfit_residuals(
    data,
    y_name,
    x,
    tuned_g$final_spec,
    folds_outer,
    g_factory
  )

  # final stage
  fin <- tibble::tibble(res_y = res_y, res_d = res_d)
  ols <- stats::lm(res_y ~ 0 + res_d, data = fin)
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
