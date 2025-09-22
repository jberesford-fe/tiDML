<!-- badges: start -->
<!-- badges: end -->

# tiDML

The goal of tiDML is twofold:

1. **Simple first pass**: provide a straightforward way to run Double Machine Learning (DML) in R. Users only need to specify their data, a formula, and a model type as string (e.g. "random forest", "xgboost" etc). Defaults are set to sensible values, so it’s a quick first pass to ask: “do my OLS results change materially under DML?”

2. **Run DML the tidymodels way**: a flexible framework that lets you define and inspect both stages of the DML process explicitly. Specify first- and second-stage models, chosing from thousands of <a href="https://www.tidymodels.org/find/parsnip/">`parsnip`</a> models. In the backend, preprocessing is handled through `recipes`, and these are combined with `workflows`. These models are easy to examine individually, and fit seamlessly into the tiDML pipeline.

In short, while tiDML can simplify things, it's main contribution is in letting you define and examine both stages of the DML process explicitly, in a way that a `tidymodels` user would know and love. 

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

random_forest <- dml_rf(
  data = df,
  y = "y_var",
  d = "d_var",
  x = c("x1", "x2", "x3"),
)

print(fit)
```

For users requiring more control (i.e. moving past the testing phase and into implementation), you can (i) define your own first- and second-stage models using `parsnip`, (ii) handle pre-processing steps, for both stages, using `recipes`, and (iii) pass both the `parsnip` model specs and `recipes` recipe to the `run_dml()` function to get a DML estimate. 

``` r

See the [vignette](https://jberesford-fe.github.io/tiDML/articles/tiDML.html) for more detailed examples.