#' Out-of-fold cross-fitting for DML
#'
#' @keywords internal
oof_crossfit <- function(data, folds, m_fit_fun, g_fit_fun, y_name, d_name) {
  n <- nrow(data)
  g_hat <- numeric(n)
  m_hat <- numeric(n)

  # treated level for classification probs
  tlev <- treated_level(data[[d_name]])
  d_is_factor <- is.factor(data[[d_name]])

  for (i in seq_len(nrow(folds))) {
    analysis_idx <- folds$splits[[i]]$in_id
    assessment_idx <- setdiff(seq_len(n), analysis_idx)

    train <- data[analysis_idx, , drop = FALSE]
    test <- data[assessment_idx, , drop = FALSE]

    # fit nuisances on train
    m_fit <- m_fit_fun(train)
    g_fit <- g_fit_fun(train)

    # predict g (outcome regression) on test
    g_pred <- stats::predict(g_fit, new_data = test)
    g_hat[assessment_idx] <- dplyr::pull(
      g_pred,
      tidyselect::starts_with(".pred")
    )

    # predict m (treatment model) on test
    if (d_is_factor) {
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
      m_pred <- stats::predict(m_fit, new_data = test)
      m_hat[assessment_idx] <- dplyr::pull(
        m_pred,
        tidyselect::starts_with(".pred")
      )
    }
  }

  y_res <- data[[y_name]] - g_hat
  d_res <- if (d_is_factor) {
    # factor → numeric {0,1}
    as.numeric(data[[d_name]] == tlev) - m_hat
  } else {
    as.numeric(data[[d_name]]) - m_hat
  }

  list(
    y_res = y_res,
    d_res = d_res,
    g_hat = g_hat,
    m_hat = m_hat
  )
}
