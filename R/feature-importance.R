#' @export
get_feature_importance <- function(
  dml_result,
  model = c("treatment", "outcome")
) {
  model <- match.arg(model)

  fits <- if (model == "treatment") dml_result$m_fits else dml_result$g_fits

  if (is.null(fits)) {
    stop(
      "Models were not stored. Refit with store_models = TRUE",
      call. = FALSE
    )
  }

  importance_list <- lapply(seq_along(fits), function(i) {
    fit <- fits[[i]]
    engine <- workflows::extract_fit_engine(fit)

    if (!is.null(engine$variable.importance)) {
      tibble::tibble(
        fold_rep = dml_result$folds$id[[i]],
        variable = names(engine$variable.importance),
        importance = unname(engine$variable.importance)
      )
    } else {
      NULL
    }
  })

  return(dplyr::bind_rows(importance_list))
}
