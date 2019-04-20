library(tidytext)
library(tidyverse)
library(tmcn)
library(stringr)
library(jiebaR)
library(tm)
library(stopwords)
library(caret) #ML
library(randomForest)
library(e1071)
library(LiblineaR) #svm
library(rpart)

#data loading

train1 <-read_csv("Classification.csv") # the original
train1$Markers <- NULL # removing uncoded posts

any(is.na(train1$Text))
any(is.na(train1$Text))

t1 <- train1[complete.cases(train1), ]

trainset <- t1
####


#### creating dummay for each variable:
trainset$Anger <- as.integer(str_detect(t1$Tag,"Anger"))
trainset$Hawk <- as.integer(str_detect(t1$Tag,"Hawkish"))
trainset$Dove <- as.integer(str_detect(t1$Tag,"Dovish"))
trainset$Protest <- as.integer(str_detect(t1$Tag,"Protests and collective action"))
trainset$Information <- as.integer(str_detect(t1$Tag,"Information"))
trainset$Power <- as.integer(str_detect(t1$Tag,"Power"))
trainset$News <- as.integer(str_detect(t1$Tag,"News"))
trainset$Irrelevant<-as.integer(str_detect(t1$Tag,"Irrelevant"))
trainset$ID <- rownames(trainset) #document ID
trainset$ID <- as.integer(trainset$ID)
trainset$Tag <- NULL

trainset$Anger <- as.factor(trainset$Anger)
trainset$Hawk <- as.factor(trainset$Hawk)
trainset$Dove <- as.factor(trainset$Dove)
trainset$Protest <- as.factor(trainset$Protest)
trainset$Information <- as.factor(trainset$Information )
trainset$Power <- as.factor(trainset$Power)
trainset$News <- as.factor(trainset$News)
trainset$Irrelevant<- as.factor(trainset$Irrelevant)


####

summary(trainset)

stop_words_cn <- as.tibble(stopwords(language = "zh", source = "misc"))
names(stop_words_cn)[1] <- "word"

#Create a tidy format 

##       ID  cong billnum  h_or_sen major word       
##    <int> <int>   <int> <fct>    <int> <chr>      
##  1     1   107    4499 HR          18 suspend    
##  2     1   107    4499 HR          18 temporarili

trainset_tokens <- trainset %>%
  unnest_tokens(output = word, input = Text) 


#%>%
 # anti_join(stop_words_cn)

# pre-subsample descriptive stats
trainset_tokens %>%
  keep(is.factor) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_bar()                         # as density

#jibaR segment and tokenize:


# # dealing with class imblance:






# library(DMwR)
# 
# trainset_tokens_df<- hellno::as.data.frame(trainset_tokens, char)
# trainset_tokens_df$word <- as.factor(trainset_tokens_df$word)
# trainset_tokens_df$ID <- as.integer(trainset_tokens_df$ID)
# 
# 
# set.seed(2019)
# smote_train <- SMOTE(Anger ~ ., data  = trainset_tokens_df)                         
# table(smote_train$Anger) 
# 
# smote_tokens<-as_tibble(smote_train)
# smote_tokens$word <-as.character(smote_tokens$word)


#ploting discriptive stats
smote_tokens %>%
  keep(is.factor) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_bar()                         # as density


#Create document-term matrix
#Tidy text data frames are one-row-per-token, but for statistical learning algorithms
#we need our data in a one-row-per-document format.
#That is, a document-term matrix. We can use cast_dtm() to create a document-term matrix.

trainset_dtm <- trainset_tokens %>%
  # get count of each token in each document
  count(ID, word) %>%
  # create a document-term matrix with all features and tf weighting
  cast_dtm(document = ID, term = word, value = n)

trainset_dtm <- removeSparseTerms(trainset_dtm, sparse = .995)





##             ############ Estimate models ######################

model_weights <- ifelse(trainset$Anger == 1,
                        (1/table(imbal_train$Class)[1]) * 0.5,
                        (1/table(imbal_train$Class)[2]) * 0.5)


#try a random forest model first # not working due to imblance
train_rf <- train(x = as.matrix(trainset_dtm),
                  y = factor(trainset$Anger),
                  method = "rf",
                  ntree = 200,
                  trControl = trainControl(method = "oob"),
                  weights = model_weights)


# Support Vector Machine with weights: svmLinearWeights



#################

#train test-split
meta <- tibble(ID = as.numeric(dimnames(trainset_dtm)[[1]])) %>%
  left_join(trainset[!duplicated(trainset$ID), ], by = "ID")

set.seed(2019)
trainIndex <- createDataPartition(trainset$Anger, p = 0.8, list = FALSE, times = 1)

data_df_train <- trainset_dtm[trainIndex, ] %>% as.matrix() %>% as.data.frame()
data_df_test <- trainset_dtm[-trainIndex, ] %>% as.matrix() %>% as.data.frame()

response_train <- meta$Anger[trainIndex]

################ #Support Vector Machine Accuracy : 0.8119  

trctrl <- trainControl(method = "none")

svm_mod <- train(x = data_df_train,
                 y = as.factor(response_train),
                 method = "svmLinearWeights2",
                 trControl = trctrl,
                 tuneGrid = data.frame(cost = 1, 
                                       Loss = 0, 
                                       weight = 1))

svm_pred <- predict(svm_mod,
                    newdata = data_df_test)

svm_cm <- confusionMatrix(svm_pred, meta[-trainIndex, ]$Anger)
svm_cm

#tuning hyperparameter
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 3,
                           search = "grid")


# tuned svm Accuracy : 0.8465 
svm_mod_tuning <- train(x = data_df_train,
                 y = as.factor(response_train),
                 method = "svmLinearWeights2",
                 trControl = fitControl,
                 tuneGrid = data.frame(cost = 0.01, 
                                       Loss = 0, 
                                       weight = seq(0.5, 1.5, 0.1)))

svm_pred_tuned <- predict(svm_mod_tuning,
                    newdata = data_df_test)

svm_cm_tuned <- confusionMatrix(svm_pred_tuned, meta[-trainIndex, ]$Anger)
svm_cm_tuned


#we can plot the train object to see which value is highest.
plot(svm_mod_tuned)



#                     LogitBoost Accuracy : 0.7772
logitboost_mod <- train(x = data_df_train,
                        y = as.factor(response_train),
                        method = "LogitBoost",
                        trControl = trctrl)
logitboost_pred <- predict(logitboost_mod,
                           newdata = data_df_test)

logitboost_cm <- confusionMatrix(logitboost_pred, meta[-trainIndex, ]$Anger)
logitboost_cm

# Random forest #Accuracy : 0.8416  
rf_mod <- train(x = data_df_train, 
                y = as.factor(response_train), 
                method = "ranger",
                trControl = trctrl,
                tuneGrid = data.frame(mtry = floor(sqrt(dim(data_df_train)[2])),
                                      splitrule = "gini",
                                      min.node.size = 1))

rf_pred <- predict(rf_mod,
                   newdata = data_df_test)

rf_cm <- confusionMatrix(rf_pred, meta[-trainIndex, ]$Anger)
rf_cm

