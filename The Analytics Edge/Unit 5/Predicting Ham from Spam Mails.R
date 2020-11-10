#Spam from Ham

#Required Libraries
library(tidyverse)
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)


#Loading the data
Email_Data <- read_csv(file = "emails.csv")

#Lets look at the data
str(Email_Data)


#Lets see the emails
table(Email_Data$spam)


#Ham Emails
Email_Data %>%
  filter(spam == 0)

#Longest mail
max(nchar(Email_Data$text))

#Row with the shortest text
which.min(nchar(Email_Data$text))


#Building a Corpus
Corpus <- Corpus(VectorSource(Email_Data$text))

#Converting test to lowercase

Corpus <- tm_map(Corpus, tolower)

#Removing Punctuations

Corpus <- tm_map(Corpus, removePunctuation)


#Removing stop words
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))

##Stem Document
Corpus <- tm_map(Corpus, stemDocument)

Spam_dtm <- DocumentTermMatrix(Corpus)


#More Sparse Matrix
Sparse_dtm <- removeSparseTerms(Spam_dtm, 0.95)

#Building a datafram with sparse dtm
Emails_Sparse <- as_tibble(as.matrix(Sparse_dtm))

#Word appearing the most
which.max(colSums(Emails_Sparse))

#Adding the spam variable
Emails_Sparse$spam <- Email_Data$spam

#In the Ham emails how many words appear atleat 5000 time
Ham <- Emails_Sparse %>%
  filter(spam == 0)
Ham[ ,colSums(Ham) >= 5000]

#In the Spam emails how many words appear atleat 5000 time
Spam <- Emails_Sparse %>%
  filter(spam == 1)
Spam[ ,colSums(Spam) >= 5000]



#Converting dependent Var to factor
Emails_Sparse$spam <- as.factor(Emails_Sparse$spam)

#Setting random seed
set.seed(123)


Split_Index <- sample.split(Emails_Sparse$spam, 0.7)


Emails_Sparse_Train <- Emails_Sparse[Split_Index, ]

Emails_Sparse_Test <- Emails_Sparse[!Split_Index, ]


#LogModel
Spam_LogModel <- glm(formula = spam ~. , data = Emails_Sparse_Train,
                     family = "binomial")


#CART Model
Spam_CART <- rpart(formula = spam ~., data = Emails_Sparse_Train,
                   method = "class")



#Random Forrest Model





