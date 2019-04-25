
#train test-split Dove
set.seed(2019)
trainIndex_d <- createDataPartition(trainset$Dove, p = 0.8, list = FALSE, times = 1)

data_df_train_dove <- trainset_dtm[trainIndex_d, ] %>% as.matrix() %>% as.data.frame()
data_df_test_dove <- trainset_dtm[-trainIndex_d, ] %>% as.matrix() %>% as.data.frame()

response_train_dove <- meta$Dove[trainIndex_d]

#tuning hyperparameter
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 3,
                           search = "grid")

# tuned svm Accuracy Dove model:  0.9703         
svm_dove <- train(x = data_df_train_dove,
                  y = as.factor(response_train_dove),
                  method = "svmLinearWeights2",
                  trControl = fitControl,
                  tuneGrid = data.frame(cost = 2, 
                                        Loss = 0, 
                                        weight = seq(0.5, 1.5, 0.1)))

plot(svm_dove)


svm_pred_dove <- predict(svm_dove,
                         newdata = data_df_test_dove)

svm_cm_dove <- confusionMatrix(svm_pred_dove, meta[-trainIndex_d, ]$Dove)
svm_cm_dove
