#' DML-PLR with LASSO (glmnet)
#' @param data Data frame
#' @param y Outcome column
#' @param d Treatment column
#' @param x Covariate columns
#' @param folds_outer Optional rsample rset. If NULL, folds made internally (stratified on D).
#' @param n_folds Number of outer folds when `folds_outer` is NULL.
#' @param n_rep Number of repetitions when `folds_outer` is NULL.
#' @param vcov_type Sandwich variance type (e.g., "HC2" or "HC3").
#' @param penalty_m Optional single penalty (lambda) for the m-model (treatment).
#' @param penalty_g Optional single penalty (lambda) for the g-model (outcome).
#' @param mixture Elastic net mixing parameter (0 = ridge, 1 = lasso).
#' @param store_models If TRUE, keep fitted nuisance models inside the result.
#' @export
dml_enet <- function(
  data,
  y,
  d,
  x,
  folds_outer = NULL,
  n_folds = 5,
  n_rep = 1,
  vcov_type = "HC2",
  penalty_m = 0.01,
  penalty_g = 0.01,
  mixture = 0.5,
  penalties_grid = NULL,
  store_models = FALSE
) {
  y_name <- rlang::as_name(rlang::ensym(y))
  d_name <- rlang::as_name(rlang::ensym(d))
  x <- vapply(x, rlang::as_name, character(1))

  treatment_type <- get_treatment_type(data[[d_name]])

  # ---- model specs ----------------------------------------------------------
  # m-model: treatment
  if (treatment_type == "binary_factor") {
    m_spec <- parsnip::logistic_reg(
      penalty = penalty_m,
      mixture = mixture
    ) |>
      parsnip::set_mode("classification") |>
      parsnip::set_engine("glmnet")
  } else {
    m_spec <- parsnip::linear_reg(
      penalty = penalty_m,
      mixture = mixture
    ) |>
      parsnip::set_mode("regression") |>
      parsnip::set_engine("glmnet")
  }

  # g-model: outcome
  g_spec <- parsnip::linear_reg(
    penalty = penalty_g,
    mixture = mixture
  ) |>
    parsnip::set_mode("regression") |>
    parsnip::set_engine("glmnet")

  # ---- recipes & workflows --------------------------------------------------
  m_rec <- make_m_recipe(data, d_name = d_name, x = x)
  g_rec <- make_g_recipe(data, y_name = y_name, x = x)

  m_wf <- make_workflow(m_spec, m_rec)
  g_wf <- make_workflow(g_spec, g_rec)

  # ---- core DML routine -----------------------------------------------------
  dml_core_wf(
    data = data,
    m_wf = m_wf,
    g_wf = g_wf,
    folds_outer = folds_outer,
    n_folds = n_folds,
    n_rep = n_rep,
    vcov_type = vcov_type,
    store_models = store_models
  )
}
