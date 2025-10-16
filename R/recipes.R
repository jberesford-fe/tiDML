#' @keywords internal
#' @importFrom rlang .data
recipe_outcome_name <- function(rec, label = "recipe") {
  nm <- summary(rec) |>
    dplyr::filter(.data$role == "outcome") |>
    dplyr::pull("variable")

  if (length(nm) != 1L) {
    stop(label, " must have exactly one outcome.", call. = FALSE)
  }
  nm
}

#' @keywords internal
make_m_recipe <- function(data, d_name, x) {
  base_recipe <- recipes::recipe(
    stats::reformulate(termlabels = x, response = d_name),
    data = data
  )
  base_recipe
}

#' @keywords internal
make_g_recipe <- function(data, y_name, x) {
  recipes::recipe(
    stats::reformulate(termlabels = x, response = y_name),
    data = data
  )
}

#' @keywords internal
make_workflow <- function(spec, rec) {
  workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_recipe(rec)
}

#' @keywords internal
pick_trees_by_oob <- function(df, rec, spec_base, grid, fallback) {
  if (is.null(grid) || length(grid) == 0L) {
    return(fallback)
  }

  errs <- numeric(length(grid))

  for (i in seq_along(grid)) {
    # Update the trees argument in the model spec
    spec_i <- parsnip::set_args(spec_base, trees = grid[i])

    wf_i <- make_workflow(spec_i, rec)
    fit_i <- parsnip::fit(wf_i, data = df)
    errs[i] <- oob_error(fit_i)
  }

  grid[which.min(errs)]
}
