#' Core DML-PLR using workflows
#' @export
#' @importFrom workflows pull_workflow_preprocessor
dml_core_wf <- function(
  data,
  m_wf,
  g_wf,
  folds_outer = NULL,
  n_folds = 5,
  n_rep = 1,
  vcov_type = "HC2",
  store_models = FALSE
) {
  m_rec <- workflows::extract_preprocessor(m_wf)
  g_rec <- workflows::extract_preprocessor(g_wf)

  if (!inherits(m_rec, "recipe") || !inherits(g_rec, "recipe")) {
    stop("Both workflows must use recipes as preprocessors.", call. = FALSE)
  }

  d_name <- recipe_outcome_name(m_rec, "treatment_recipe")
  y_name <- recipe_outcome_name(g_rec, "output_recipe")

  assert_cols(data, c(y_name, d_name))
  if (!is.numeric(data[[y_name]])) {
    stop("Outcome `", y_name, "` must be numeric.", call. = FALSE)
  }
  if (!is_valid_treatment(data[[d_name]])) {
    stop(
      "Treatment `",
      d_name,
      "` must be either binary factor or numeric with variation.",
      call. = FALSE
    )
  }

  treatment_type <- get_treatment_type(data[[d_name]])

  if (is.null(folds_outer)) {
    folds_outer <- make_folds_stratified(
      data,
      d = d_name,
      n_folds = n_folds,
      n_rep = n_rep
    )
  }

  m_fit_fun <- function(df) parsnip::fit(m_wf, data = df)
  g_fit_fun <- function(df) parsnip::fit(g_wf, data = df)

  cf <- oof_crossfit(
    data = data,
    folds = folds_outer,
    m_fit_fun = m_fit_fun,
    g_fit_fun = g_fit_fun,
    y_name = y_name,
    d_name = d_name,
    store_models = store_models
  )
  inf <- plr_estimate(cf$y_res, cf$d_res, vcov_type = vcov_type)

  structure(
    list(
      estimate = unname(inf$theta),
      se = unname(inf$se),
      ci_95 = unname(inf$ci),
      y_res = cf$y_res,
      d_res = cf$d_res,
      g_hat = cf$g_hat,
      m_hat = cf$m_hat,
      folds = folds_outer,
      reps = n_rep,
      m_fits = cf$m_fits,
      g_fits = cf$g_fits,
      lm_fit = inf$lm_fit,
      vcov = inf$vcov,
      vcov_type = vcov_type,
      .y_orig = data[[y_name]],
      .d_orig = data[[d_name]],
      treatment_type = treatment_type,
      m_workflow = m_wf,
      g_workflow = g_wf,
      call = match.call()
    ),
    class = "dml_plr",
    y_name = y_name,
    d_name = d_name
  )
}
