#' Run DML-PLR with user-supplied recipes and parsnip models
#'
#' @param data Data frame used for fitting and cross-fitting.
#' @param output_recipe A \code{recipes::recipe} with a single numeric outcome (Y) and predictors (X).
#' @param treatment_recipe A \code{recipes::recipe} with a single binary outcome (D) and predictors (X).
#' @param output_model A parsnip model spec for the outcome regression g(X) (mode = "regression").
#' @param treatment_model A parsnip model spec for the treatment model m(X) (mode = "classification" for factor D, or "regression" for numeric {0,1} D).
#' @param folds_outer rsample rset used for cross-fitting (e.g., \code{make_folds(data, v=5)}).
#' @param vcov_type Sandwich variance type (e.g., "HC2").
#'
#' @return list with estimate, SE, CI, residuals, predictions, and audit objects.
#' @export
run_dml <- function(
  data,
  output_recipe,
  treatment_recipe,
  output_model,
  treatment_model,
  folds_outer = make_folds(data, v = 5),
  vcov_type = "HC2"
) {
  # ---- validate inputs ----
  if (!inherits(output_recipe, "recipe")) {
    stop("`output_recipe` must be a recipes::recipe.", call. = FALSE)
  }
  if (!inherits(treatment_recipe, "recipe")) {
    stop("`treatment_recipe` must be a recipes::recipe.", call. = FALSE)
  }
  if (!inherits(output_model, "model_spec")) {
    stop("`output_model` must be a parsnip model spec.", call. = FALSE)
  }
  if (!inherits(treatment_model, "model_spec")) {
    stop("`treatment_model` must be a parsnip model spec.", call. = FALSE)
  }

  # Extract outcome names from recipes
  # Extract outcome names from recipes (via term_info attr)
  out_info <- attr(output_recipe, "term_info")
  trt_info <- attr(treatment_recipe, "term_info")

  y_name <- summary(output_recipe) |>
    dplyr::filter(role == "outcome") |>
    dplyr::pull(variable)

  d_name <- summary(treatment_recipe) |>
    dplyr::filter(role == "outcome") |>
    dplyr::pull(variable)

  if (length(y_name) != 1L) {
    stop("`output_recipe` must have exactly one outcome.", call. = FALSE)
  }
  if (length(d_name) != 1L) {
    stop("`treatment_recipe` must have exactly one outcome.", call. = FALSE)
  }

  # Basic checks
  assert_cols(data, c(y_name, d_name))
  if (!is.numeric(data[[y_name]])) {
    stop("Outcome `", y_name, "` must be numeric.", call. = FALSE)
  }
  if (!is_binary_like(data[[d_name]])) {
    stop(
      "Treatment `",
      d_name,
      "` must be binary-like (factor with 2 levels or numeric {0,1}).",
      call. = FALSE
    )
  }

  # Determine treatment type and ensure treatment_model mode fits
  d_is_factor <- is.factor(data[[d_name]])
  tm_mode <- treatment_model$mode
  gm_mode <- output_model$mode

  if (d_is_factor && !identical(tm_mode, "classification")) {
    warning(
      "`treatment_model` mode is not 'classification' but treatment is a factor; attempting to proceed.",
      immediate. = TRUE
    )
  }
  if (!d_is_factor && !identical(tm_mode, "regression")) {
    warning(
      "`treatment_model` mode is not 'regression' but treatment is numeric {0,1}; attempting to proceed.",
      immediate. = TRUE
    )
  }
  if (!identical(gm_mode, "regression")) {
    warning(
      "`output_model` mode should be 'regression' for numeric Y; attempting to proceed.",
      immediate. = TRUE
    )
  }

  # Build workflows from provided specs + recipes
  m_wf <- workflows::workflow() |>
    workflows::add_model(treatment_model) |>
    workflows::add_recipe(treatment_recipe)

  g_wf <- workflows::workflow() |>
    workflows::add_model(output_model) |>
    workflows::add_recipe(output_recipe)

  # Cross-fitting: fit per fold on analysis split, predict on assessment
  m_fit_fun <- function(df) parsnip::fit(m_wf, data = df)
  g_fit_fun <- function(df) parsnip::fit(g_wf, data = df)

  cf <- oof_crossfit(
    data = data,
    folds = folds_outer,
    m_fit_fun = m_fit_fun,
    g_fit_fun = g_fit_fun,
    y_name = y_name,
    d_name = d_name
  )

  # PLR inference
  inf <- plr_estimate(cf$y_res, cf$d_res, vcov_type = vcov_type)

  # Return
  list(
    estimate = inf$theta,
    se = inf$se,
    ci_95 = inf$ci,
    y_res = cf$y_res,
    d_res = cf$d_res,
    g_hat = cf$g_hat,
    m_hat = cf$m_hat,
    folds = folds_outer,
    output_recipe = output_recipe,
    treatment_recipe = treatment_recipe,
    output_model = output_model,
    treatment_model = treatment_model,
    lm_fit = inf$lm_fit,
    vcov = inf$vcov
  )
}
