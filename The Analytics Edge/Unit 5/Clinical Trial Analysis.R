#Clinical Trial Analysis

#Required Packages
library(tidyverse)
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)

#Load the data
Clinical_Trial_Data <- read_csv(file = "clinical_trial.csv")

#Lets look at the Data
str(Clinical_Trial_Data)
summary(Clinical_Trial_Data)


#Longest Abstract
max(nchar(Clinical_Trial_Data$abstract), na.rm = TRUE)


#Search results with NO abstract
sum(is.na(Clinical_Trial_Data$abstract))

#minimum number of texts in the title field
min_index <- which.min(nchar(Clinical_Trial_Data$title))
Clinical_Trial_Data$title[min_index]


#Building the Corpora
Corpus_Title <- Corpus(VectorSource(Clinical_Trial_Data$title))

Corpus_Abstract <- Corpus(VectorSource(Clinical_Trial_Data$abstract))


#Converting to Lower case
Corpus_Title <- tm_map(Corpus_Title, tolower)

Corpus_Abstract <- tm_map(Corpus_Abstract, tolower)

#Removing punctuation
Corpus_Title <- tm_map(Corpus_Title, removePunctuation)

Corpus_Abstract <- tm_map(Corpus_Abstract, removePunctuation)

#Removing stop words
Corpus_Title <- tm_map(Corpus_Title, removeWords, stopwords("english"))

Corpus_Abstract <- tm_map(Corpus_Abstract, removeWords, stopwords("english"))


#Stem words
Corpus_Title <- tm_map(Corpus_Title, stemDocument)

Corpus_Abstract <- tm_map(Corpus_Abstract, stemDocument)

#Building the document term matrix
dtm_Title <- DocumentTermMatrix(Corpus_Title)

dtm_Abstract <- DocumentTermMatrix(Corpus_Abstract)


#Limiting to 95% sparsity

sparse_Title <- removeSparseTerms(dtm_Title, sparse = 0.95)

sparse_Abstract <- removeSparseTerms(dtm_Abstract, sparse = 0.95)

#Converting sparse matrix to dataframes
Title_Sparse <- as_tibble(as.matrix(sparse_Title))

Abstract_Sparse <- as_tibble(as.matrix(sparse_Abstract))


#Most frequent word accross all abstracts
which.max(colSums(Abstract_Sparse))


#Lets combine the two df
colnames(Title_Sparse) = paste0("T", colnames(Title_Sparse))

colnames(Abstract_Sparse) = paste0("A", colnames(Abstract_Sparse))


#Combining the 2 df
dtm <- cbind(Title_Sparse, Abstract_Sparse)

#Adding the dependent Var
dtm$trial <- Clinical_Trial_Data$trial


#Lets build the models
set.seed(144)

#Spliting the df
Split_Index <- sample.split(dtm$trial, SplitRatio = 0.7)

dtm_Train <- dtm[Split_Index, ]

dtm_Test <- dtm[!Split_Index, ]

#Baseline Model
table(dtm_Test$trial)


#Building the Model
Trial_CART <-rpart(formula = trial ~., data = dtm_Train, 
                   method = "class")

#lot the Model
prp(Trial_CART)


#Obtaining Testing Set Predictions
Predict_CART <- predict(Trial_CART, newdata = dtm_Test)

table(dtm_Test$trial, Predict_CART)


#Model Accuracy
CART_Accuracy <- (261+162)/(261+162+52+83)

#Loading recquired packages
CART_ROCR_Pred <- prediction(Predict_CART[,2], dtm_Test$trial)
CART_ROCR_Perf <- performance(CART_ROCR_Pred, "tpr", "fpr")
plot(CART_ROCR_Perf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), 
     text.adj = c(-0.2,1.7))

AUC_Test <- as.numeric(performance(CART_ROCR_Pred, "auc")@y.values)






