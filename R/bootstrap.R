usethis::use_build_ignore("bootstrap.R")

pkgs <- c(
  "parsnip",
  "recipes",
  "workflows",
  "rsample",
  "tune",
  "yardstick",
  "dials",
  "rlang",
  "dplyr",
  "tibble",
  "tidyr",
  "purrr",
  "sandwich",
  "lmtest",
  "stats"
)
for (p in pkgs) {
  usethis::use_package(p, type = "Imports")
}


usethis::use_roxygen_md()
usethis::use_testthat()
usethis::use_git_ignore(c(".Rproj.user", ".Renviron", "inst/doc"))
usethis::use_mit_license("Justin Beresford")
