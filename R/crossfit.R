#' Out-of-fold cross-fitting for DML
#' @keywords internal
oof_crossfit <- function(data, folds, m_fit_fun, g_fit_fun, y_name, d_name) {
  n <- nrow(data)
  g_hat <- numeric(n)
  m_hat <- numeric(n)

  treatment_type <- get_treatment_type(data[[d_name]])

  # Only need treated level for binary factors
  tlev <- if (treatment_type == "binary_factor") {
    treated_level(data[[d_name]])
  } else {
    NULL
  }

  for (i in seq_len(nrow(folds))) {
    analysis_idx <- folds$splits[[i]]$in_id
    assessment_idx <- setdiff(seq_len(n), analysis_idx)

    train <- data[analysis_idx, , drop = FALSE]
    test <- data[assessment_idx, , drop = FALSE]

    # Fit nuisances on train
    m_fit <- m_fit_fun(train)
    g_fit <- g_fit_fun(train)

    # Predict g (outcome regression) on test
    g_pred <- stats::predict(g_fit, new_data = test)
    g_hat[assessment_idx] <- dplyr::pull(
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
      m_hat[assessment_idx] <- m_pred[[prob_col]]
    } else {
      # continuous treatment
      m_pred <- stats::predict(m_fit, new_data = test)
      m_hat[assessment_idx] <- dplyr::pull(
        m_pred,
        tidyselect::starts_with(".pred")
      )
    }
  }

  # Calculate residuals
  y_res <- data[[y_name]] - g_hat
  d_res <- if (treatment_type == "binary_factor") {
    # Binary factor: convert to 0/1 then subtract propensity
    as.numeric(data[[d_name]] == tlev) - m_hat
  } else {
    # Continuous: subtract predicted treatment
    as.numeric(data[[d_name]]) - m_hat
  }

  list(
    y_res = y_res,
    d_res = d_res,
    g_hat = g_hat,
    m_hat = m_hat
  )
}
