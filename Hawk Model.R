
#train test-split Hawk
set.seed(2019)
trainIndex_h <- createDataPartition(trainset$Hawk, p = 0.8, list = FALSE, times = 1)

data_df_train_hawk <- trainset_dtm[trainIndex_h, ] %>% as.matrix() %>% as.data.frame()
data_df_test_hawk <- trainset_dtm[-trainIndex_h, ] %>% as.matrix() %>% as.data.frame()

response_train_hawk <- meta$Hawk[trainIndex_h]

#tuning hyperparameter
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 3,
                           search = "grid")

# tuned svm Accuracy Hawk model:  0.8515                     
svm_hawk <- train(x = data_df_train_hawk,
                        y = as.factor(response_train_hawk),
                        method = "svmLinearWeights2",
                        trControl = fitControl,
                        tuneGrid = data.frame(cost = 1.5, 
                                              Loss = 0, 
                                              weight = seq(0.5, 1.5, 0.1)))

plot(svm_hawk)

svm_pred_hawk <- predict(svm_hawk,
                          newdata = data_df_test_hawk)

svm_cm_hawk <- confusionMatrix(svm_pred_hawk, meta[-trainIndex_h, ]$Hawk)
svm_cm_hawk

