
#train test-split Power
set.seed(2019)
trainIndex_p <- createDataPartition(trainset$Power, p = 0.8, list = FALSE, times = 1)

data_df_train_Power <- trainset_dtm[trainIndex_p, ] %>% as.matrix() %>% as.data.frame()
data_df_test_Power <- trainset_dtm[-trainIndex_p, ] %>% as.matrix() %>% as.data.frame()

response_train_Power <- meta$Power[trainIndex_p]

#tuning hyperparameter
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 3,
                           search = "grid")

# tuned svm Accuracy Power model:  0.8515                     
svm_Power <- train(x = data_df_train_Power,
                  y = as.factor(response_train_Power),
                  method = "svmLinearWeights2",
                  trControl = fitControl,
                  tuneGrid = data.frame(cost = 1.5, 
                                        Loss = 0, 
                                        weight = seq(0.5, 1.5, 0.1)))

plot(svm_Power)

svm_pred_Power <- predict(svm_Power,
                         newdata = data_df_test_Power)

svm_cm_Power <- confusionMatrix(svm_pred_Power, meta[-trainIndex_p, ]$Power)
svm_cm_Power

