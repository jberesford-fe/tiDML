set.seed(123)
n <- 1500
x1 <- rnorm(n)
x2 <- rnorm(n)
D <- 0.5 * x1 - 0.2 * x2 + rnorm(n)
Y <- 2.0 * D + 0.3 * x1 + 0.1 * x2 + rnorm(n)
dat <- tibble::tibble(Y, D, x1, x2)


fit <- dml_plr(
  df = dat,
  y = Y,
  d = D,
  x = c("x1", "x2"),
  m_model = "rf", # ranger RF
  g_model = "xgb" # xgboost
)

print(fit)
