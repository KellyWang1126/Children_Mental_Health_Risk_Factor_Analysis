library(randomForest)
library(xtable)

# OTHER MODELS
# logistic regression
model.logistic <- glm(KidMentalIssue ~., data = df_train, family = binomial)
model.logistic.probs <- predict(model.logistic, df_test[, -1], type = 'response')
model.logistic.pred <- rep(0, nrow(df_test))
model.logistic.pred[model.logistic.probs > 0.5] <- 1

# random forest
start.time <- Sys.time()
model.rf <- randomForest(KidMentalIssue ~. , data = df_train, 
                         mtry = round(sqrt(ncol(df_train))), importance = T,
                         ntree = 500)
end.time <- Sys.time()
round(end.time - start.time, 2)
time.taken
model.rf.pred <- predict(model.rf, df_test[, -1], type = 'response')


# EVALUATION
# F1 
ls.f1 <- c(F1_Score(df_test[, 1], xgb.logistic.pred), 
           F1_Score(df_test[, 1], xgb.hinge.pred),
           F1_Score(df_test[, 1], model.logistic.pred),
           F1_Score(df_test[, 1], model.rf.pred))

# AUC
ls.auc <- c(AUC(df_test[, 1], xgb.logistic.pred), 
            AUC(df_test[, 1], xgb.hinge.pred), 
            AUC(df_test[, 1], model.logistic.pred),
            AUC(df_test[, 1], model.rf.pred))

# ACC
ls.acc <- c(mean(df_test[, 1] == xgb.logistic.pred), 
            mean(df_test[, 1] == xgb.hinge.pred),
            mean(df_test[, 1] == model.logistic.pred), 
            mean(df_test[, 1] == model.rf.pred))

# print table
df.evaluation <- data.frame(ls.f1, ls.auc, ls.acc)
names(df.evaluation) <- c("F1", "AUC", "ACC")
rownames(df.evaluation) <- c("XGBoost: Logistic Loss", "XGBoost: Hinge Loss",
                             "Logistic Regression", "Random Forest")


xtable(df.evaluation, digits = 5, align = c("l", "l", "l", "l"))

