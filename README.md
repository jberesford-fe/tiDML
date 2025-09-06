<!-- badges: start -->
<!-- badges: end -->

# tiDML

The goal of tiDML is twofold:
1. Provide an ultra user-friendly way of implementing Double Machine Learning (DML) models in R, only defining the data and the model (random forest, glm or ...). All parameters default to sensible values and this can be used as a first pass to answer "do my OLS results change materially if I switch to DML".


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

