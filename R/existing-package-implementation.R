library(DoubleML)
library(mlr3)
library(mlr3learners)
library(paradox)
library(partykit)
library(rpart)
library(partykit)
library(caret)


set.seed(42)

n <- 500
df <- data.frame(
  X1 = rnorm(n),
  X2 = rnorm(n),
  D = rbinom(n, 1, 0.5)
)
linpred <- -0.3 + 0.7 * df$X1 - 1.2 * df$X2 + 0.5 * df$D
p <- plogis(linpred)
df$Y <- rbinom(n, 1, p)


dml_df <- DoubleML::DoubleMLData$new(
  df,
  y_col = "Y",
  d_cols = "D",
  x_cols = c("X1", "X2")
)

learner <- lrn(
  "classif.rpart",
  predict_type = "prob",
  cp = 0.001,
  keep_model = TRUE
)

dml_plr <- DoubleMLPLR$new(
  data = dml_df,
  ml_l = learner$clone(),
  ml_m = learner$clone()
)

dml_plr$fit(store_predictions = TRUE, store_models = TRUE)

model_l <- dml_plr$models$ml_l
model_m <- dml_plr$models$ml_m

rf_y <- model_l[[1]][[1]][[1]]$model
rf_y$variable.importance

as.party(rf_y, data = TRUE) |>
  plot(main = "Regression Tree for Y")


preds_prob <- predict(rf_y, newdata = df)
preds_class <- ifelse(preds_prob > 0.5, 1, 0)
table(Predicted = preds_class, Actual = df$Y)

caret::confusionMatrix(
  factor(preds_class, levels = c(0, 1)),
  factor(df$Y, levels = c(0, 1))
)


rf_d <- dml_plr$models$ml_m[[1]][[1]][[1]]$model
preds_d_prob <- predict(rf_d, newdata = df)
preds_d_class <- ifelse(preds_d_prob > 0.5, 1, 0)
confusionMatrix(
  factor(preds_d_class, levels = c(0, 1)),
  factor(df$D, levels = c(0, 1))
)
