library(DoubleML)
library(mlr3)
library(mlr3learners)
library(paradox)
library(partykit)
library(rpart)
library(partykit)
library(caret)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)

set.seed(42)

####### Make some synthetic (binary categorical) data #######
n <- 500
df <- data.frame(
  X1 = rnorm(n),
  X2 = rnorm(n),
  D = rbinom(n, 1, 0.5)
)
linpred <- -0.3 + 0.7 * df$X1 - 1.2 * df$X2 + 0.5 * df$D
p <- plogis(linpred)
df$Y <- rbinom(n, 1, p)


df |>
  ggplot(aes(x = X1, y = X2, color = factor(D))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = D)) +
  labs(title = "Scatter plot of the synthetic data") +
  theme_minimal()

####### Set up DoubleML data structure #######
dml_df <- DoubleML::DoubleMLData$new(
  df,
  y_col = "Y",
  d_cols = "D",
  x_cols = c("X1", "X2")
)


####### Explore mlr3 learners #######

# There are 15 classification learners
as.data.table(mlr_learners) |>
  as_tibble() |>
  filter(task_type == "classif") |>
  select(-predict_types) |>
  unnest_wider(properties, names_sep = "_")

# And 12 regression learners
as.data.table(mlr_learners) |>
  as_tibble() |>
  filter(task_type == "regr") |>
  select(-predict_types) |>
  unnest_wider(properties, names_sep = "_")


###### Set up one of the classification learners #######
as.data.table(mlr_learners) |>
  as_tibble() |>
  filter(key == "regr.ranger") |>
  unnest_wider(predict_types, names_sep = "_")

decision_tree_learner <- lrn(
  "regr.rpart",
  predict_type = "response",
  keep_model = TRUE
)


random_forest_learner <- lrn(
  "regr.ranger",
  predict_type = "response",
  num.trees = 3,
  min.node.size = 4,
  importance = "impurity"
)

####### Fit DoubleML PLR model #######

dml_plr <- DoubleMLPLR$new(
  data = dml_df,
  ml_l = random_forest_learner$clone(),
  ml_m = random_forest_learner$clone()
)

dml_plr$fit(store_predictions = TRUE, store_models = TRUE)


######### Check results for phase 1 model (estimate of D) #######

# Check out the fold-level results
# For the first fold (of 5) and repetition (of 1). There's only 1 treatment var.
model_m <- dml_plr$models$ml_m

rep <- 1
treatment <- 1
fold <- 3

# Print OOB R^2 to the console - how do I pull it out?
model_m[[rep]][[treatment]][[fold]]$model

# Variable/feature importance for the first fold and repetition
model_m[[rep]][[treatment]][[fold]]$importance()


model_m[[1]][[1]][[5]]$model

######## Extract overall model results ########
# Now we know how to get them for single folds, let's get the overall results

get_feature_importance <- function(model, rep, treatment, fold) {
  model[[rep]][[treatment]][[fold]]$importance()
}

get_feature_importance(model_m, rep = 1, treatment = 1, fold = 1)

folds <- 1:length(model_m[[1]][[1]])
map_dfr(folds, ~ get_feature_importance(model_m, rep = 1, treatment = 1, .x))


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
