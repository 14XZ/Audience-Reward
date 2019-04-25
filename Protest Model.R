
#train test-split Protest
set.seed(2019)
trainIndex_prot <- createDataPartition(trainset$Protest, p = 0.8, list = FALSE, times = 1)

data_df_train_protest <- trainset_dtm[trainIndex_prot, ] %>% as.matrix() %>% as.data.frame()
data_df_test_protest <- trainset_dtm[-trainIndex_prot, ] %>% as.matrix() %>% as.data.frame()

response_train_protest <- meta$Protest[trainIndex_prot]

#tuning hyperparameter
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 3,
                           search = "grid")

# tuned svm Accuracy Protest model:  0.9257                        
svm_protest <- train(x = data_df_train_protest,
                        y = as.factor(response_train_protest),
                        method = "svmLinearWeights2",
                        trControl = fitControl,
                        tuneGrid = data.frame(cost = 1.5, 
                                              Loss = 0, 
                                              weight = seq(0.5, 1.5, 0.1)))

plot(svm_protest)

svm_pred_protest <- predict(svm_protest,
                               newdata = data_df_test_protest)

svm_cm_protest <- confusionMatrix(svm_pred_protest, meta[-trainIndex_prot, ]$Protest)
svm_cm_protest

