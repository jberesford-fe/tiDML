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
      final_spec = workflows::extract_spec_parsnip(wf),
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
  metric_name <- if (spec$mode == "regression") "rmse" else "roc_auc"

  res <- tune::tune_grid(
    wf,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    control = tune::control_grid(save_pred = FALSE)
  )

  best <- tune::select_best(res, metric = metric_name)
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
    return(predict(fitted_wf, new_data)[[1]])
  }

  prob <- predict(fitted_wf, new_data, type = "prob")

  if (!ncol(prob)) {
    stop(
      "Classifier returned no probability columns. Did you set probability = TRUE for ranger?"
    )
  }

  # Default to the second probability column if not specified
  if (is.null(positive_class)) {
    col <- colnames(prob)[2L]
  } else {
    # Accept either ".pred_<level>" or "<level>"
    col <- if (positive_class %in% colnames(prob)) {
      positive_class
    } else {
      paste0(".pred_", positive_class)
    }
  }

  if (!col %in% colnames(prob)) {
    stop(
      "Positive class column '",
      col,
      "' not found. Available: ",
      paste(colnames(prob), collapse = ", ")
    )
  }

  prob[[col]]
}


#' Cross-fit residuals across outer folds
#' @keywords internal
crossfit_residuals <- function(
  data,
  outcome,
  predictors,
  spec,
  folds_idx,
  recipe_factory
) {
  outcome_name <- if (is.character(outcome) && length(outcome) == 1) {
    outcome
  } else {
    rlang::as_name(rlang::ensym(outcome))
  }
  y_is_factor <- is.factor(data[[outcome_name]])

  # Decide positive class if factor
  pos_level <- if (y_is_factor) levels(data[[outcome_name]])[2L] else NULL

  res <- rep(NA_real_, nrow(data))
  for (te_idx in folds_idx) {
    tr_idx <- setdiff(seq_len(nrow(data)), te_idx)
    tr <- data[tr_idx, , drop = FALSE]
    te <- data[te_idx, , drop = FALSE]

    rec <- recipe_factory(tr, outcome_name, predictors)
    wf <- workflows::workflow() |>
      workflows::add_recipe(rec) |>
      workflows::add_model(spec)

    fit <- workflows::fit(wf, tr)
    pred <- numeric_pred(fit, te, positive_class = pos_level)

    obs_num <- if (y_is_factor) {
      as.integer(te[[outcome_name]] == pos_level)
    } else {
      te[[outcome_name]]
    }
    res[te_idx] <- obs_num - pred
  }
  if (anyNA(res)) {
    stop("Missing residuals for some rows.")
  }
  res
}
