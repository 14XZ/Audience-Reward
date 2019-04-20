library(tidyverse)
library(caret)
library(dataPreparation)


# tokenize new data 
relevant_tokens <- relevantposts %>%
  unnest_tokens(output = word, input = text)


#transfom relevant posts into a document-term matrix
#Tidy text data frames are one-row-per-token, but for statistical learning algorithms
#we need our data in a one-row-per-document format.
#That is, a document-term matrix. We can use cast_dtm() to create a document-term matrix.

relevant_dtm <- relevant_tokens %>%
  # get count of each token in each document
  count(mid, word) %>%
  # create a document-term matrix with all features and tf weighting
  cast_dtm(document = mid, term = word, value = n)

relevant_dtm <- removeSparseTerms(relevant_dtm, sparse = .991)

new_relevant <- relevant_dtm %>% as.matrix() %>% as.data.frame()


# RTextTools

svm_classification <- predict(svm_mod_tuning,
                          newdata = new_relevant)


findFreqTerms(data_df_train, 600)


###
str(data_df_train)
str(new_relevant)

