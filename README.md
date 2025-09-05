
# tiDML

<!-- badges: start -->
<!-- badges: end -->

The goal of tiDML is to ...

## Installation

You can install the development version of tiDML from [GitHub](https://github.com/) with:

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

