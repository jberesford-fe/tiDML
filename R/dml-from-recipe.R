#' Run DML-PLR with user-supplied recipes and parsnip models
#' @export
#' @param data Data frame
#' @param outcome_recipe Recipe for outcome (g) model
#' @param treatment_recipe Recipe for treatment (m) model
#' @param outcome_model parsnip model spec for outcome (g) model
#' @param treatment_model parsnip model spec for treatment (m) model
#' @param folds_outer Optional rsample rset.
#' @param n_folds Number of outer folds when `folds_outer` is NULL.
#' @param n_rep Number of repetitions when `folds_outer` is NULL.
#' @param vcov_type Sandwich variance type (e.g., "HC2" or "HC3").
#' @param store_models If TRUE, keep fitted nuisance models inside the result
run_dml <- function(
  data,
  outcome_recipe,
  treatment_recipe,
  outcome_model,
  treatment_model,
  folds_outer = NULL,
  n_folds = 5,
  n_rep = 1,
  vcov_type = "HC2",
  store_models = FALSE
) {
  if (!inherits(outcome_recipe, "recipe")) {
    stop("`outcome_recipe` must be a recipe.", call. = FALSE)
  }
  if (!inherits(treatment_recipe, "recipe")) {
    stop("`treatment_recipe` must be a recipe.", call. = FALSE)
  }
  if (!inherits(outcome_model, "model_spec")) {
    stop("`outcome_model` must be a parsnip spec.", call. = FALSE)
  }
  if (!inherits(treatment_model, "model_spec")) {
    stop("`treatment_model` must be a parsnip spec.", call. = FALSE)
  }

  g_wf <- workflows::workflow() |>
    workflows::add_model(outcome_model) |>
    workflows::add_recipe(outcome_recipe)

  m_wf <- workflows::workflow() |>
    workflows::add_model(treatment_model) |>
    workflows::add_recipe(treatment_recipe)

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
