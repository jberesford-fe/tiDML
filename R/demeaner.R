demean_panel <- function(
  data,
  unit,
  time,
  vars = tidyselect::where(is.numeric),
  suffix = "_dm",
  replace = FALSE,
  na.rm = TRUE
) {
  # tidy-eval capture
  unit <- rlang::ensym(unit)
  time <- rlang::ensym(time)

  # basic checks
  if (!rlang::as_name(unit) %in% names(data)) {
    stop("`unit` column not found in `data`.", call. = FALSE)
  }
  if (!rlang::as_name(time) %in% names(data)) {
    stop("`time` column not found in `data`.", call. = FALSE)
  }

  # which variables to transform
  var_cols <- tidyselect::eval_select(rlang::enquo(vars), data = data)
  if (!length(var_cols)) {
    stop("No columns selected in `vars`.", call. = FALSE)
  }
  var_names <- names(var_cols)

  # overall means (named list for quick lookup)
  overall_means <- data |>
    dplyr::summarise(dplyr::across(
      dplyr::all_of(var_names),
      ~ mean(.x, na.rm = na.rm)
    ))
  overall_means <- as.list(overall_means[1, , drop = TRUE])

  # unit means
  unit_means <- data |>
    dplyr::group_by(!!unit) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(var_names),
        ~ mean(.x, na.rm = na.rm),
        .names = "{.col}__unit_mean"
      ),
      .groups = "drop"
    )

  # time means
  time_means <- data |>
    dplyr::group_by(!!time) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(var_names),
        ~ mean(.x, na.rm = na.rm),
        .names = "{.col}__time_mean"
      ),
      .groups = "drop"
    )

  # join means and compute de-meaned columns
  out <- data |>
    dplyr::left_join(
      unit_means,
      by = rlang::set_names(rlang::as_name(unit), rlang::as_name(unit))
    ) |>
    dplyr::left_join(
      time_means,
      by = rlang::set_names(rlang::as_name(time), rlang::as_name(time))
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(var_names),
        ~ {
          u <- get(paste0(cur_column(), "__unit_mean"))
          t <- get(paste0(cur_column(), "__time_mean"))
          # x_it - x_i. - x_.t + x_..
          .x - u - t + overall_means[[cur_column()]]
        },
        .names = if (replace) "{.col}" else paste0("{.col}", suffix)
      )
    ) |>
    dplyr::select(
      -tidyselect::ends_with("__unit_mean"),
      -tidyselect::ends_with("__time_mean")
    )

  out
}
