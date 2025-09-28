<!-- badges: start -->
<!-- badges: end -->

# tiDML

The goal of tiDML is twofold:

1. **Simple first pass**: provide a straightforward way to run Double Machine Learning (DML) in R. Users only need to pick a model, then specify their data and a formula. Defaults are set to sensible values, so it’s a quick first pass to ask: **“do my OLS results change materially under DML?”**

2. **Run DML the tidymodels way**: a flexible framework that lets you define and inspect both stages of the DML process explicitly. Specify first- and second-stage models, chosing from thousands of <a href="https://www.tidymodels.org/find/parsnip/">`parsnip`</a> models, then specify your own preprocessing steps through `recipes`. In the backend, these are combined into `workflows`, which are used for the first- and second-stage models in a DML partially linear regression.

In short, while tiDML simplifies some default models, it's main contribution is in letting you define and examine both stages of the DML process explicitly, in the way that a `tidymodels` user would expect. 

## Installation

You can install the development version of tiDML from [GitHub](https://github.com/) via `pak`, `pacman` or `remotes`:

``` r 
# Using pak (recommended)
pak::pak("jberesford-fe/tiDML")

# using remotes
remotes::install_github("jberesford-fe/tiDML")

# not currently working:
pacman::p_load_gh("jberesford-fe/tiDML")

```

## Simple Example

This is the most basic example which shows you how to run a DML PLR model with random forests for both nuisance models, taking all parameters as default.

``` r
library(tiDML)

random_forest <- dml_rf(
  data = df,
  y = "y_var",
  d = "d_var",
  x = c("x1", "x2", "x3"),
)

print(random_forest)
```
# Explicit defintion

For users requiring more control (i.e. moving past the testing phase and into implementation), you can 

1. Define your own first- and second-stage models using [`parsnip`](https://parsnip.tidymodels.org/). For exmample, using random forests for both stages:

```{r parsnip_models, eval=FALSE}
outcome_model <- parsnip::rand_forest(trees = 500) |>
  parsnip::set_engine("ranger") |>
  parsnip::set_mode("regression")

treatment_model <- parsnip::rand_forest(trees = 500) |>
  parsnip::set_engine("ranger") |>
  parsnip::set_mode("classification")
```

2. Handle any pre-processing steps, for both stages, using [`recipes`](https://recipes.tidymodels.org/). For example, standardising all numeric predictors, turning nominals to dummies, or log-transforming the outcome variable:

```{r recipes, eval=FALSE}
treatment_recipe <- recipes::recipe(d_var ~ x1 + x2 + x3, data = df) |>
  recipes::step_normalize(recipes::all_numeric_predictors()) |>
  recipes::step_dummy(recipes::all_nominal_predictors()) |>
  recipes::step_log(recipes::all_outcomes(), base = 10)

outcome_recipe <- recipes::recipe(y_var ~ x1 + x2 + x3, data = df) |>
  recipes::step_center(all_numeric_predictors()) |>
  recipes::step_scale(all_numeric_predictors()) 
```


3. Pass both the `parsnip` model specs and `recipes` recipe to the `run_dml()` function to get a DML estimate. Noting that the same data must be passed to `run_dml()` as was used to define the recipes.  

```{r run_dml, eval=FALSE}
run_dml(
  data = df,
  outcome_model = outcome_model,
  treatment_model = treatment_model,
  outcome_recipe = outcome_recipe,
  treatment_recipe = treatment_recipe,
  n_folds = 5
)
```

See the [getting started](https://jberesford-fe.github.io/tiDML/articles/tiDML.html) vignette for more detailed examples.