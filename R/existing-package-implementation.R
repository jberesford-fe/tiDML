library(DoubleML)
library(mlr3)
library(mlr3learners)
library(paradox)
library(partykit)
library(rpart)
library(partykit)

df <- load_example_data()
dml_df <- DoubleML::DoubleMLData$new(
  df,
  y_col = "Y",
  d_cols = "D",
  x_cols = c("X1", "X2")
)

learner <- lrn("regr.rpart", cp = 0.001, keep_model = TRUE)


dml_plr <- DoubleMLPLR$new(
  data = dml_df,
  ml_l = learner$clone(),
  ml_m = learner$clone()
)


dml_plr$models$ml_l

dml_plr$fit(store_predictions = TRUE, store_models = TRUE)

model_l <- dml_plr$models$ml_l
model_m <- dml_plr$models$ml_m


rf_y <- dml_plr$models$ml_l[[1]][[1]][[1]]$model
rf_y$variable.importance


library(rpart.plot)
rpart.plot(
  rf_y,
  type = 3,
  fallen.leaves = TRUE,
  roundint = FALSE,
  cex = 1.0, # 20 % larger text and boxes
  branch.lwd = 2 # thicker lines
)


party_tree <- as.party(rf_y, data = TRUE)
plot(party_tree, main = "Regression Tree for Y")
