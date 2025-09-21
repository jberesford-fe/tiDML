#' DML-PLR (Partially Linear Regression) with random forest default
#'
#' @param data Data frame
#' @param y Outcome column
#' @param d Treatment column (binary-like: factor with 2 levels, or {0,1})
#' @param x Character vector of predictors
#' @param folds_outer Outer folds (shared by both nuisances) for cross-fitting
#' @param vcov_type Sandwich variance type (e.g., "HC2")
#' @return list with estimate, SE, CI, and audit objects
#' @export
dml_rf <- function(
  data,
  y,
  d,
  x,
  v = 5,
  folds_outer = NULL,
  vcov_type = "HC2",
  trees_grid = NULL
) {
  # -------- resolve names --------
  y_sym <- rlang::ensym(y)
  d_sym <- rlang::ensym(d)
  y_name <- rlang::as_name(y_sym)
  d_name <- rlang::as_name(d_sym)
  x <- vapply(x, rlang::as_name, character(1))

  # -------- basic checks --------
  assert_cols(data, c(y_name, d_name, x))
  if (!is.numeric(data[[y_name]])) {
    stop("`y` must be numeric.", call. = FALSE)
  }
  if (!is_binary_like(data[[d_name]])) {
    stop(
      "`d` must be binary-like (factor with 2 levels or numeric {0,1}).",
      call. = FALSE
    )
  }

  # ----- set folds -----

  # -------- folds --------
  if (is.null(folds_outer)) {
    # stratify if d is binary
    if (is_binary_like(data[[d_name]])) {
      folds_outer <- make_folds_stratified(data, d = d_name, v = v)
    } else {
      folds_outer <- make_folds(data, v = v)
    }
  }

  # -------- set mtry defaults --------
  p <- length(x)
  d_is_factor <- is.factor(data[[d_name]])

  mtry_m <- if (d_is_factor) max(1, floor(sqrt(p))) else max(1, floor(p / 3))
  mtry_g <- max(1, floor(p / 3))

  # -------- base RF specs --------
  m_spec <- parsnip::rand_forest(trees = 500, mtry = mtry_m) |>
    parsnip::set_mode(if (d_is_factor) "classification" else "regression") |>
    parsnip::set_engine("ranger", num.threads = 1, probability = d_is_factor)

  g_spec <- parsnip::rand_forest(trees = 500, mtry = mtry_g) |>
    parsnip::set_mode("regression") |>
    parsnip::set_engine("ranger", num.threads = 1)

  # -------- recipes & workflows --------
  m_rec <- make_m_recipe(data, d_name = d_name, x = x)

  g_rec <- make_g_recipe(data, y_name = y_name, x = x)
  # ------- make workflow from rec and spec ------
  m_wf <- make_workflow(m_spec, m_rec)
  g_wf <- make_workflow(g_spec, g_rec)

  # -------- tune trees by OOB error --------
  trees_m <- pick_trees_by_oob(
    df = data,
    rec = m_rec,
    spec_base = m_spec,
    grid = trees_grid,
    fallback = 500
  )

  trees_g <- pick_trees_by_oob(
    df = data,
    rec = g_rec,
    spec_base = g_spec,
    grid = trees_grid,
    fallback = 500
  )

  # update workflows with chosen trees
  m_wf <- m_wf |>
    workflows::update_model(parsnip::set_args(m_spec, trees = trees_m))

  g_wf <- g_wf |>
    workflows::update_model(parsnip::set_args(g_spec, trees = trees_g))

  # -------- cross-fitting --------
  m_fit_fun <- function(df) parsnip::fit(m_wf, data = df)
  g_fit_fun <- function(df) parsnip::fit(g_wf, data = df)

  cf <- oof_crossfit(
    data = data,
    folds = folds_outer,
    m_fit_fun = m_fit_fun,
    g_fit_fun = g_fit_fun,
    y_name = y_name,
    d_name = d_name
  )

  # -------- inference --------
  inf <- plr_estimate(cf$y_res, cf$d_res, vcov_type = vcov_type)

  # -------- return --------

  return(
    structure(
      list(
        estimate = unname(inf$theta),
        se = unname(inf$se),
        ci_95 = unname(inf$ci),
        y_res = cf$y_res,
        d_res = cf$d_res,
        g_hat = cf$g_hat,
        m_hat = cf$m_hat,
        folds = folds_outer,
        mtry_m = mtry_m,
        mtry_g = mtry_g,
        trees_m = trees_m,
        trees_g = trees_g,
        lm_fit = inf$lm_fit,
        vcov = inf$vcov,
        .y_orig = data[[y_name]],
        .d_orig = data[[d_name]]
      ),
      class = "dml_plr",
      y_name = y_name,
      d_name = d_name
    )
  )
}
