<!-- badges: start -->
<!-- badges: end -->

# tiDML

The goal of tiDML is twofold:

1. **Simple first pass**: provide a straightforward way to run Double Machine Learning (DML) in R. Users only need to specify their data, a formula, and a model type as string (e.g. "random forest", "xgboost" etc). Defaults are set to sensible values, so it’s a quick first pass to ask: “do my OLS results change materially under DML?”

2. **Run DML the tidymodels way**: a flexible framework that lets you define and inspect both stages of the DML process explicitly. Specify first- and second-stage models with `parsnip`, handle preprocessing through `recipes`, and combine them with `workflows`. These custom nuisance models then fit seamlessly into the tiDML pipeline.


In short, while tiDML can simplify things, it's main contribution is in letting you do DML explicitly, the way a `tidymodels` user would expect. 

## Installation

You can install the development version of tiDML from [GitHub](https://github.com/) via `pak`, `pacman` or `remotes`:

``` r
# Using pak
pak::pak("jberesford-fe/tiDML")

# using pacman
pacman::p_load_gh("jberesford-fe/tiDML")

# using remotes
remotes::install_github("jberesford-fe/tiDML")
```

## Example

This is the most basic example which shows you how to run a DML PLR model with random forests for both nuisance models, taking all parameters as default.

``` r
library(tiDML)

fit <- dml_plr(
  data = mtcars,
  y = "mpg",
  d = "am",
  x = c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "gear", "carb"),
  m_model = "rf",
  g_model = "rf"
) 

print(fit)
```

