#' Out-of-fold cross-fitting for DML
#' @keywords internal
oof_crossfit <- function(
  data,
  folds,
  m_fit_fun,
  g_fit_fun,
  y_name,
  d_name,
  store_models = FALSE
) {
  n <- nrow(data)
  n_splits <- nrow(folds)
  print(n_splits)

  # Accumulate predictions across repeats
  g_hat <- numeric(n)
  m_hat <- numeric(n)
  count_vec <- integer(n)

  # Storage for fitted models if requested
  m_fits <- if (store_models) vector("list", n_splits) else NULL
  g_fits <- if (store_models) vector("list", n_splits) else NULL

  treatment_type <- get_treatment_type(data[[d_name]])
  tlev <- if (treatment_type == "binary_factor") {
    treated_level(data[[d_name]])
  } else {
    NULL
  }

  for (i in seq_len(n_splits)) {
    analysis_idx <- folds$splits[[i]]$in_id
    assessment_idx <- setdiff(seq_len(n), analysis_idx)

    train <- data[analysis_idx, , drop = FALSE]
    test <- data[assessment_idx, , drop = FALSE]

    # Fit nuisances on train
    m_fit <- m_fit_fun(train)
    g_fit <- g_fit_fun(train)

    # Store fitted models if requested
    if (store_models) {
      m_fits[[i]] <- m_fit
      g_fits[[i]] <- g_fit
    }

    # Predict g (outcome regression) on test
    g_pred <- stats::predict(g_fit, new_data = test)
    g_hat[assessment_idx] <- g_hat[assessment_idx] +
      dplyr::pull(
        g_pred,
        tidyselect::starts_with(".pred")
      )

    # Predict m (treatment model) on test
    if (treatment_type == "binary_factor") {
      m_pred <- stats::predict(m_fit, new_data = test, type = "prob")
      prob_col <- paste0(".pred_", tlev)
      if (!prob_col %in% names(m_pred)) {
        stop(
          "Could not find probability column ",
          prob_col,
          " in m-model predictions.",
          call. = FALSE
        )
      }
      m_hat[assessment_idx] <- m_hat[assessment_idx] + m_pred[[prob_col]]
    } else {
      m_pred <- stats::predict(m_fit, new_data = test)
      m_hat[assessment_idx] <- m_hat[assessment_idx] +
        dplyr::pull(
          m_pred,
          tidyselect::starts_with(".pred")
        )
    }

    count_vec[assessment_idx] <- count_vec[assessment_idx] + 1
  }

  # Average predictions across repeats
  g_hat <- g_hat / count_vec
  m_hat <- m_hat / count_vec

  # Calculate residuals
  y_res <- data[[y_name]] - g_hat
  d_res <- if (treatment_type == "binary_factor") {
    as.numeric(data[[d_name]] == tlev) - m_hat
  } else {
    as.numeric(data[[d_name]]) - m_hat
  }

  list(
    y_res = y_res,
    d_res = d_res,
    g_hat = g_hat,
    m_hat = m_hat,
    m_fits = m_fits,
    g_fits = g_fits
  )
}
