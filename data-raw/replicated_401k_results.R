# data-raw/generate_401k_replications.R
# Generates inst/extdata/dml401k_replications.rds for the DoubleML replication vignette.
# Run locally only (NOT_CRAN=true) — takes ~4h with reps=100.

# Dependencies:
#   tiDML, DoubleML, mlr3, mlr3learners, dplyr, purrr, tibble

# Output:
#   inst/extdata/dml401k_replications.rds## code to prepare `replicated_401k_results` dataset goes here

library(tiDML)
library(DoubleML)
library(dplyr)
library(purrr)
library(tibble)
library(mlr3)
library(mlr3learners)

df401k <- DoubleML::fetch_401k(return_type = "data.frame", instrument = FALSE)

# DoubmleML Function
run_dml <- function(seed, df, y, d, x, trees_grid, n_folds, n_rep) {
  set.seed(seed)
  ## DoubleML :: PLR with ranger
  ml_l <- lrn(
    "regr.ranger",
    num.trees = trees_grid,
    num.threads = 1,
    respect.unordered.factors = "order"
  )

  ml_m <- lrn(
    "classif.ranger",
    num.trees = trees_grid,
    num.threads = 1,
    predict_type = "prob"
  )

  dml_data <- DoubleMLData$new(
    data = df,
    y_col = y,
    d_cols = d,
    x_cols = x
  )

  dml <- DoubleMLPLR$new(
    dml_data,
    ml_l = ml_l,
    ml_m = ml_m,
    n_folds = n_folds,
    n_rep = n_rep,
    score = "partialling out"
  )

  dml$fit()

  ci <- dml$confint(level = 0.95)
  return(tibble(
    method = "DoubleML",
    seed = seed,
    theta = as.numeric(dml$coef),
    se = as.numeric(dml$se),
    lwr = as.numeric(ci[1]),
    upr = as.numeric(ci[2])
  ))
}


# tiDML Function
run_tidml <- function(seed, df, y, d, x, trees_grid, n_folds, n_rep) {
  set.seed(seed)

  df <- df |> mutate(!!d := as.factor(!!rlang::sym(d)))

  fit_tidml <- dml_rf(
    data = df,
    y = !!rlang::sym(y),
    d = !!rlang::sym(d),
    x = x,
    trees_grid = trees_grid,
    n_folds = n_folds,
    n_rep = n_rep,
  )

  return(
    tibble(
      method = "tiDML",
      seed = seed,
      theta = unname(fit_tidml$estimate),
      se = unname(fit_tidml$se)
    ) |>
      mutate(
        lwr = theta - stats::qnorm(0.975) * se,
        upr = theta + stats::qnorm(0.975) * se
      )
  )
}

# Wrapper to run both methods

run_both <- function(
  seed,
  df,
  y,
  d,
  x,
  trees_grid = 1200,
  n_folds = 2L,
  n_rep = 1L
) {
  tidml_row <- run_tidml(seed, df, y, d, x, trees_grid, n_folds, n_rep)
  dml_row <- run_dml(seed, df, y, d, x, trees_grid, n_folds, n_rep)
  bind_rows(tidml_row, dml_row)
}


set.seed(401)
reps <- 5L
seeds <- 401 + 0:(reps - 1L)
dml401k_replications <- map_dfr(
  seeds,
  ~ run_both(
    .x,
    df = df401k,
    y = "net_tfa",
    d = "e401",
    x = c(
      "age",
      "inc",
      "educ",
      "fsize",
      "marr",
      "twoearn",
      "db",
      "pira",
      "hown"
    ),
    trees_grid = 1200,
    n_folds = 2L,
    n_rep = 1L
  )
)

usethis::use_data(
  dml401k_resreplications,
  internal = TRUE,
  compress = "xz",
  overwrite = TRUE
)


load("R/sysdata.rda")
res
