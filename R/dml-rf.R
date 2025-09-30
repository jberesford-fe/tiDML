#' DML-PLR with Random Forest
#' @param data Data frame
#' @param y Outcome column
#' @param d Treatment column
#' @param x Covariate columns
#' @param folds_outer Optional rsample rset. If NULL, folds made internally (stratified on D).
#' @param n_folds Number of outer folds when `folds_outer` is NULL.
#' @param n_rep Number of repetitions when `folds_outer` is NULL.
#' @param vcov_type Sandwich variance type (e.g., "HC2" or "HC3").
#' @param trees_grid Optional grid of number of trees to try (for both m- and g-models).
#' @export
dml_rf <- function(
  data,
  y,
  d,
  x,
  folds_outer = NULL,
  n_folds = 5,
  n_rep = 1,
  vcov_type = "HC2",
  trees_grid = NULL,
  store_models = FALSE
) {
  y_name <- rlang::as_name(rlang::ensym(y))
  d_name <- rlang::as_name(rlang::ensym(d))
  x <- vapply(x, rlang::as_name, character(1))

  p <- length(x)
  treatment_type <- get_treatment_type(data[[d_name]])

  mtry_m <- if (treatment_type == "binary_factor") {
    max(1, floor(sqrt(p))) # Classification default
  } else {
    max(1, floor(p / 3)) # Regression default
  }
  mtry_g <- max(1, floor(p / 3)) # Outcome var is always regression

  m_spec <- parsnip::rand_forest(trees = 500, mtry = mtry_m) |>
    parsnip::set_mode(
      if (treatment_type == "binary_factor") "classification" else "regression"
    ) |>
    parsnip::set_engine(
      "ranger",
      num.threads = 1,
      probability = (treatment_type == "binary_factor"),
      importance = if (store_models) "impurity" else "none"
    )

  g_spec <- parsnip::rand_forest(trees = 500, mtry = mtry_g) |>
    parsnip::set_mode("regression") |>
    parsnip::set_engine(
      "ranger",
      num.threads = 1,
      importance = if (store_models) "impurity" else "none"
    )

  m_rec <- make_m_recipe(data, d_name = d_name, x = x)
  g_rec <- make_g_recipe(data, y_name = y_name, x = x)

  m_wf <- make_workflow(m_spec, m_rec)
  g_wf <- make_workflow(g_spec, g_rec)

  if (!is.null(trees_grid) && length(trees_grid)) {
    trees_m <- pick_trees_by_oob(
      data,
      m_rec,
      m_spec,
      trees_grid,
      fallback = 500
    )
    trees_g <- pick_trees_by_oob(
      data,
      g_rec,
      g_spec,
      trees_grid,
      fallback = 500
    )
    m_wf <- workflows::update_model(
      m_wf,
      parsnip::set_args(m_spec, trees = trees_m)
    )
    g_wf <- workflows::update_model(
      g_wf,
      parsnip::set_args(g_spec, trees = trees_g)
    )
  }

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
