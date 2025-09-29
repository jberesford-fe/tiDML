#' DML-PLR with single-hidden-layer MLP (nnet engine)
#'
#' Uses a small MLP for both nuisances. Adds dummy encoding for nominal X
#' and normalizes numeric predictors (recommended for NNs).
#'
#' @param data,y,d,x As in dml_rf()
#' @param folds_outer Optional rsample rset. If NULL, folds made internally (stratified on D).
#' @param n_folds Number of outer folds when `folds_outer` is NULL.
#' @param n_rep Number of repetitions when `folds_outer` is NULL.
#' @param vcov_type Sandwich variance type (e.g., "HC2" or "HC3").
#' @param hidden_units_m,hidden_units_g Hidden units for m- and g-models (NULL = heuristic).
#' @param penalty L2 penalty (a.k.a. weight decay).
#' @param epochs Max iterations (passed to `nnet::nnet()` via parsnip).
#' @param max_weights Max allowable weights for nnet engine (`MaxNWts`) to avoid overflow.
#' @param trace Logical; print nnet training trace.
#' @export
dml_nnet <- function(
  data,
  y,
  d,
  x,
  folds_outer = NULL,
  n_folds = 5,
  n_rep = 1,
  vcov_type = "HC2",
  hidden_units_m = NULL,
  hidden_units_g = NULL,
  penalty = 0.001,
  epochs = 200,
  max_weights = 5000,
  trace = FALSE
) {
  # --- resolve names ---
  y_name <- rlang::as_name(rlang::ensym(y))
  d_name <- rlang::as_name(rlang::ensym(d))
  x <- vapply(x, rlang::as_name, character(1))

  # --- treatment type & simple heuristics ---
  p <- length(x)
  treatment_type <- get_treatment_type(data[[d_name]])

  if (is.null(hidden_units_m)) {
    hidden_units_m <- max(2L, round(sqrt(p)))
  }
  if (is.null(hidden_units_g)) {
    hidden_units_g <- max(2L, round(sqrt(p) + 1L))
  }

  # --- parsnip specs ---
  m_spec <- parsnip::mlp(
    hidden_units = hidden_units_m,
    penalty = penalty,
    epochs = epochs
  ) |>
    parsnip::set_mode(
      if (treatment_type == "binary_factor") "classification" else "regression"
    ) |>
    parsnip::set_engine("nnet", trace = trace, MaxNWts = max_weights)

  g_spec <- parsnip::mlp(
    hidden_units = hidden_units_g,
    penalty = penalty,
    epochs = epochs
  ) |>
    parsnip::set_mode("regression") |>
    parsnip::set_engine("nnet", trace = trace, MaxNWts = max_weights)

  # --- recipes: dummy encode nominals, normalize numerics, drop zero-var ---
  m_rec <- make_m_recipe(data, d_name = d_name, x = x) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  g_rec <- make_g_recipe(data, y_name = y_name, x = x) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  m_wf <- make_workflow(m_spec, m_rec)
  g_wf <- make_workflow(g_spec, g_rec)

  # --- delegate to core ---
  dml_core_wf(
    data = data,
    m_wf = m_wf,
    g_wf = g_wf,
    folds_outer = folds_outer,
    n_folds = n_folds,
    n_rep = n_rep,
    vcov_type = vcov_type
  )
}
