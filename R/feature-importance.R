#' Get feature importance from nuisance models
#' @export
#' @param dml_result A DML result object with stored nuisance models.
#' @param model Which nuisance model coefficients to extract, either "treatment" or "outcome".
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

  importance <- lapply(seq_along(fits), function(i) {
    fit <- fits[[i]]
    engine <- workflows::extract_fit_engine(fit)

    if (!is.null(engine$variable.importance)) {
      tibble::tibble(
        rep = dml_result$folds$id[[i]],
        fold = dml_result$folds$id2[[i]],
        variable = names(engine$variable.importance),
        importance = unname(engine$variable.importance)
      )
    } else {
      NULL
    }
  })

  return(dplyr::bind_rows(importance))
}

#' Get feature coefficients from nuisance models
#' @export
#' @importFrom dplyr mutate
#' @importFrom purrr map_dfr
#' @importFrom generics tidy
#'
#' @param dml_result A DML result object with stored nuisance models.
#' @param model Which nuisance model coefficients to extract, either "treatment" or "outcome".
get_feature_coefs <- function(dml_result, model = c("treatment", "outcome")) {
  model <- match.arg(model)

  fits <- switch(
    model,
    treatment = dml_result$m_fits,
    outcome = dml_result$g_fits
  )

  stopifnot(!is.null(fits))

  purrr::map_dfr(seq_along(fits), function(i) {
    generics::tidy(fits[[i]]) |>
      dplyr::mutate(
        fold = i,
        model = model,
        .before = 1
      )
  })
}
