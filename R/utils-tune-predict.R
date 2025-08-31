#' @keywords internal
ensure_mode <- function(spec, outcome_col) {
  mode <- spec$mode %||%
    if (is.numeric(outcome_col)) "regression" else "classification"
  parsnip::set_mode(spec, mode)
}

#' @keywords internal
auto_grid <- function(spec, data_x, size = 15) {
  ps <- try(parsnip::parameters(spec), silent = TRUE)
  if (inherits(ps, "try-error") || nrow(ps) == 0) {
    return(NULL)
  }
  suppressWarnings(ps <- dials::finalize(ps, data_x))
  dials::grid_max_entropy(ps, size = size)
}

#' Tune any parsnip spec on given resamples with a recipe
#' @keywords internal
tune_any <- function(
  spec,
  rec,
  resamples,
  data_x,
  metrics = NULL,
  grid_size = 15
) {
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(spec)
  grid <- auto_grid(spec, data_x, size = grid_size)
  if (is.null(grid)) {
    return(list(
      final_wf = wf,
      final_spec = spec,
      tune_results = NULL,
      best = NULL
    ))
  }
  if (is.null(metrics)) {
    metrics <- if (spec$mode == "regression") {
      yardstick::metric_set(yardstick::rmse)
    } else {
      yardstick::metric_set(yardstick::roc_auc)
    }
  }
  res <- tune::tune_grid(
    wf,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    control = tune::control_grid(save_pred = FALSE)
  )
  best <- tune::select_best(res, metric = metrics$.metrics[[1]])
  wf_final <- tune::finalize_workflow(wf, best)
  list(
    final_wf = wf_final,
    final_spec = workflows::extract_spec_parsnip(wf_final),
    tune_results = res,
    best = best
  )
}

#' Coerce predictions to numeric vector (regression or class prob)
#' @keywords internal
numeric_pred <- function(fitted_wf, new_data, positive_class = NULL) {
  mode <- workflows::extract_spec_parsnip(fitted_wf)$mode
  if (mode == "regression") {
    return(stats::predict(fitted_wf, new_data)[[1]])
  }
  prob <- stats::predict(fitted_wf, new_data, type = "prob")
  if (ncol(prob) == 1) {
    return(prob[[1]])
  }
  if (is.null(positive_class)) {
    positive_class <- colnames(prob)[1]
  }
  prob[[positive_class]]
}

#' Cross-fit residuals across outer folds
#' @keywords internal
crossfit_residuals <- function(
  df,
  outcome,
  predictors,
  spec,
  folds,
  recipe_factory
) {
  out <- rlang::ensym(outcome)
  res <- rep(NA_real_, nrow(df))
  for (i in seq_along(folds$splits)) {
    split <- folds$splits[[i]]
    tr <- rsample::analysis(split)
    te <- rsample::assessment(split)
    rec <- recipe_factory(tr, !!out, predictors)
    wf <- workflows::workflow() |>
      workflows::add_recipe(rec) |>
      workflows::add_model(spec)
    fit_wf <- parsnip::fit(wf, tr)
    pred <- numeric_pred(fit_wf, te)
    idx <- as.integer(rownames(te))
    res[idx] <- dplyr::pull(te, !!out) - pred
  }
  res
}
