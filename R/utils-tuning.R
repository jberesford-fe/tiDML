oob_error <- function(fitted_wf) {
  mod <- workflows::extract_fit_parsnip(fitted_wf)$fit
  pe <- mod$prediction.error

  return(pe)
}
