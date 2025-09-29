#' Run DML-PLR with user-supplied recipes and parsnip models
#' @export
run_dml <- function(
  data,
  outcome_recipe,
  treatment_recipe,
  outcome_model,
  treatment_model,
  folds_outer = NULL,
  n_folds = 5,
  n_rep = 1,
  vcov_type = "HC2"
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
    vcov_type = vcov_type
  )
}
