#' @keywords internal
has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

#' Resolve engine and sensible args for a model kind
#' @keywords internal
resolve_engine <- function(kind, mode) {
  stacks <- list(
    rf = c("ranger", "randomForest"),
    xgb = c("xgboost", "lightgbm"),
    glmnet = c("glmnet"),
    linear = c("lm", "glm")
  )
  engines <- stacks[[kind]]
  if (is.null(engines)) {
    stop("Unknown model kind: ", kind)
  }

  avail <- engines[vapply(
    engines,
    function(e) {
      pkg <- switch(
        e,
        ranger = "ranger",
        randomForest = "randomForest",
        xgboost = "xgboost",
        lightgbm = "lightgbm",
        glmnet = "glmnet",
        lm = NULL,
        glm = NULL
      )
      is.null(pkg) || has_pkg(pkg)
    },
    logical(1)
  )]
  if (!length(avail)) {
    stop(
      "No engine available for kind '",
      kind,
      "'. Install: ",
      paste(engines, collapse = ", ")
    )
  }

  eng <- avail[[1]]
  args <- list()
  if (eng == "ranger") {
    args$num.threads <- 1
    if (mode == "classification") args$probability <- TRUE
  }
  if (eng == "xgboost") {
    args$nthread <- 1
    args$tree_method <- "exact"
    args$deterministic <- TRUE
    args$single_precision_histogram <- FALSE
  }
  if (eng == "glmnet") {
    args$nthread <- 1
  }
  list(engine = eng, args = args)
}

#' Accept a parsnip spec OR a short string and return a finalized spec
#' @param model_in a parsnip model spec or one of "rf","xgb","glmnet","linear"
#' @param mode "regression" or "classification"
#' @export
resolve_spec <- function(model_in, mode) {
  mode <- match.arg(mode, c("regression", "classification"))

  if (inherits(model_in, "model_spec")) {
    if (is.null(model_in$mode) || is.na(model_in$mode)) {
      stop("Mode has not been set. Please call parsnip::set_mode(...).")
    }
    if (is.null(model_in$engine) || is.na(model_in$engine)) {
      stop("Engine has not been set. Please call parsnip::set_engine(...).")
    }
    return(model_in)
  }

  if (is.character(model_in)) {
    kind <- match.arg(model_in, c("rf", "xgb", "glmnet", "linear"))
    spec <- switch(
      kind,
      rf = parsnip::rand_forest(),
      xgb = parsnip::boost_tree(),
      glmnet = parsnip::linear_reg(),
      linear = parsnip::linear_reg()
    )
    spec <- parsnip::set_mode(spec, mode)
    eng <- resolve_engine(kind, mode)
    spec <- do.call(
      parsnip::set_engine,
      c(list(object = spec, engine = eng$engine), eng$args)
    )
    if (kind %in% c("rf", "xgb") && mode == "regression") {
      spec <- parsnip::set_args(spec, trees = 1200L)
    }
    if (kind %in% c("rf", "xgb") && mode == "classification") {
      spec <- parsnip::set_args(spec, trees = 1200L)
    }
    return(spec)
  }

  stop("model must be a parsnip spec or one of: 'rf','xgb','glmnet','linear'.")
}
