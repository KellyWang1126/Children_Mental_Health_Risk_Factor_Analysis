set.seed(1)
library(xgboost)
library(MLmetrics)
library(caret)

# convert dataframe into xgb.DMatrix
# split data into training and test data
df.xgb <- xgb.DMatrix(data.matrix(df[, -1]), 
                      label = as.numeric(df$KidMentalIssue) - 1 , nthread = 1)

train_index <- sample(1:nrow(df), round(0.7*nrow(df)))
df_train <- df[train_index, ]
df_test <- df[-train_index, ]

xgb_train <- xgb.DMatrix(data.matrix(df_train[, -1]), 
                         label = as.numeric(df_train$KidMentalIssue) - 1 , 
                         nthread = 1)
xgb_test <- xgb.DMatrix(data.matrix(df_test[, -1]), 
                        label = as.numeric(df_test$KidMentalIssue) - 1 , 
                        nthread = 1)
# XGBoost tuning
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- MLmetrics::F1_Score(y_pred = data$pred,
                                y_true = data$obs)
  c(F1 = f1_val)
}

train.control <- trainControl(method = "cv", number = 5, search = "random", 
                              summaryFunction = f1)

xgb.tuning.l <- train(KidMentalIssue~., data = df_train, method = "xgbTree",
                      objective = "binary:logistic", maximize = TRUE,
                      trControl = train.control, metric = "F1")

xgb.tuning.h <- train(KidMentalIssue~., data = df_train, method = "xgbTree",
                      objective = "binary:hinge", maximize = TRUE,
                      trControl = train.control, metric = "F1")

# XGBoost model fitting
xgb.logistic <- xgboost(learning_rate = xgb.tuning.l$bestTune[1, "eta"], 
                        nthread = 1, verbose = 0, 
                        data = xgb_train, nrounds = xgb.tuning.l$bestTune[1, "nrounds"], 
                        gamma = xgb.tuning.l$bestTune[1, "gamma"],
                        objective = "binary:logistic", booster = "gbtree", 
                        max_depth = xgb.tuning.l$bestTune[1, "max_depth"], 
                        colsample_bytree = xgb.tuning.l$bestTune[1, "colsample_bytree"], 
                        min_child_weight = xgb.tuning.l$bestTune[1, "min_child_weight"], 
                        subsample = xgb.tuning.l$bestTune[1, "subsample"])
xgb.logistic.probs <- predict(xgb.logistic, xgb_test, type = 'response')
xgb.logistic.pred <- rep(0, nrow(df_test))
xgb.logistic.pred[xgb.logistic.probs > 0.5] <- 1

# best tuning here overfitting
#xgb.hinge <- xgboost(learning_rate = xgb.tuning.h$bestTune[1, "eta"], 
#                     nthread = 1, verbose = 0, 
#                     data = xgb_train, nrounds = xgb.tuning.h$bestTune[1, "nrounds"], 
#                     gamma = xgb.tuning.h$bestTune[1, "gamma"],
#                     objective = "binary:hinge", booster = "gbtree", 
#                     max_depth = xgb.tuning.h$bestTune[1, "max_depth"], 
#                     colsample_bytree = xgb.tuning.h$bestTune[1, "colsample_bytree"], 
#                     min_child_weight = xgb.tuning.h$bestTune[1, "min_child_weight"], 
#                     subsample = xgb.tuning.h$bestTune[1, "subsample"])

xgb.hinge <- xgboost(learning_rate = xgb.tuning.h$results[1, "eta"], 
                     nthread = 1, verbose = 0, 
                     data = xgb_train, nrounds = xgb.tuning.h$results[1, "nrounds"], 
                     gamma = xgb.tuning.h$results[1, "gamma"],
                     objective = "binary:hinge", booster = "gbtree", 
                     max_depth = xgb.tuning.h$results[1, "max_depth"], 
                     colsample_bytree = xgb.tuning.h$results[1, "colsample_bytree"], 
                     min_child_weight = xgb.tuning.h$results[1, "min_child_weight"], 
                     subsample = xgb.tuning.h$results[1, "subsample"])
xgb.hinge.pred <- predict(xgb.hinge, xgb_test)

