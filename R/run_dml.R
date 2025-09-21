#' Run DML-PLR with user-supplied recipes and parsnip models
#' @export
run_dml <- function(
  data,
  output_recipe,
  treatment_recipe,
  output_model,
  treatment_model,
  folds_outer = NULL,
  v = 5,
  vcov_type = "HC2"
) {
  if (!inherits(output_recipe, "recipe")) {
    stop("`output_recipe` must be a recipe.", call. = FALSE)
  }
  if (!inherits(treatment_recipe, "recipe")) {
    stop("`treatment_recipe` must be a recipe.", call. = FALSE)
  }
  if (!inherits(output_model, "model_spec")) {
    stop("`output_model` must be a parsnip spec.", call. = FALSE)
  }
  if (!inherits(treatment_model, "model_spec")) {
    stop("`treatment_model` must be a parsnip spec.", call. = FALSE)
  }

  g_wf <- workflows::workflow() |>
    workflows::add_model(output_model) |>
    workflows::add_recipe(output_recipe)

  m_wf <- workflows::workflow() |>
    workflows::add_model(treatment_model) |>
    workflows::add_recipe(treatment_recipe)

  dml_core_wf(
    data = data,
    m_wf = m_wf,
    g_wf = g_wf,
    folds_outer = folds_outer,
    v = v,
    vcov_type = vcov_type
  )
}
