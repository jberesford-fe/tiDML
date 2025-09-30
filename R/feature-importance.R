#' @export
get_feature_importance <- function(
  dml_result,
  type = c("treatment", "outcome")
) {
  type <- match.arg(type)

  fits <- if (type == "treatment") dml_result$m_fits else dml_result$g_fits

  if (is.null(fits)) {
    stop(
      "Models were not stored. Refit with store_models = TRUE",
      call. = FALSE
    )
  }

  # Extract importance from each fold
  importance_list <- lapply(fits, function(fit) {
    engine <- workflows::extract_fit_engine(fit)
    if (!is.null(engine$variable.importance)) {
      engine$variable.importance
    } else {
      NULL
    }
  })

  importance <- dplyr::bind_rows(a, .id = "fold") |>
    dplyr::mutate(model = type)

  return(importance)
}
