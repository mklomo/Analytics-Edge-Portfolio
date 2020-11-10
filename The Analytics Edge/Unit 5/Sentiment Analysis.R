#Turning Tweets into knowledge: Text Analytics


#Textual Data: Sentiment Analysis
#The field that addresses how computers understand and derrive meaning from human
#language is called NATURAL LANGUAGE PROCESSING. 


#Creating the Textual data
#- Scraping the website
#- Use twitter's API

#To correctly classify the tweets (DEPENDENT VARIABLE), we use Amazon Mechanical Turk.
#Question asked: "Judge the sentiment expressed by the following item toward the 
#software company APPLE"

#To compute the independent variables, the bag of words algorithm is used.
#One feature for each word. Each word  is one feature.
#Preprocessing the text can dramatically improve the performance of Bag of Words.

#Preprocessing
#1 - Change text to lower case (or Upper case) so that all versions of a word will be counted separately.
#2 - Punctuation causes problems - basic approach is to remove everything.
#3 - Unhelpful terms/ "stop words" like "is, at, the, which, ..." do not improve the machine learning
#prediction quality and should be removed to reduce size of data.
#4 - Do we need to distinguish between different forms of the following words: argue, argued,
#arguing and argue? They could all be represented by a common STEM. This algorithmic reduction is called "stemming"
#Stemming is performed by:
#1 - create a database of words and their stems
#2 - write a rule-based algorithm - Porter Stemmer is popular
#3 - Train machine learning algorithms to recognize roots of words


#Packages required
library(tidyverse)
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

#Loading the textual data
Tweets_Data <- read_csv(file = "tweets.csv")

#Look at the data
str(Tweets_Data)

#Looking for negative sentimments
Tweets_Data$Negative <- as.factor(Tweets_Data$Avg <= -1)
table(Tweets_Data$Negative)


#Preprocessing the data
Tweet_Corpus <- Corpus(VectorSource(Tweets_Data$Tweet))

#Changing Text to lowercase
Tweet_Corpus <- tm_map(Tweet_Corpus, tolower)

#Remove Punctuation
Tweet_Corpus <- tm_map(Tweet_Corpus, removePunctuation)


#Removing stop words and the word "apple"
Tweet_Corpus <- tm_map(Tweet_Corpus, removeWords, c("apple", stopwords("english")))

#Lets stem our document
Tweet_Corpus <- tm_map(Tweet_Corpus, stemDocument)

#Extracting word frequencies
Frequencies <- DocumentTermMatrix(Tweet_Corpus)
inspect(Frequencies[1000:1005, 505:107]) #Sparse Data

#Looking at the most popular terms
findFreqTerms(Frequencies, lowfreq = 20)

#Lets remove terms that do not appear often
Sparse_Var <- removeSparseTerms(Frequencies, 0.995)
#0.995 means only keep terms that appear in 0.5% or more of tweets


#Converting Sparse Variable into tibble
Tweet_Sparse <- as_tibble(as.matrix(Sparse_Var))


#Making sure our column names are appropriate names
colnames(Tweet_Sparse) <- make.names(colnames(Tweet_Sparse))


#Lets add or dependent variable
Tweet_Sparse$Negative <- Tweets_Data$Negative

#Lets set seed
set.seed(123)

#Splitting variable into training and testing sets
Split_Index <- sample.split(Tweet_Sparse$Negative, SplitRatio = 0.7)

Tweet_Sparse_Train <-Tweet_Sparse[Split_Index, ]
  
Tweet_Sparse_Test <- Tweet_Sparse[!Split_Index, ]


#Using CART to build the prediction models
tweet_CART <- rpart(formula = Negative ~., data = Tweet_Sparse_Train,
                    method = "class")

#Plot the tree
prp(tweet_CART)


#Predictions
Predict_CART <- predict(tweet_CART, newdata = Tweet_Sparse_Test,
                        type = "class")

#Confusion Matrix
table(Tweet_Sparse_Test$Negative, Predict_CART)

#Model Accuracy
CART_Model_Accuracy <- (294+18)/(294+18+6+37)

#Acuuracy of baseline model
table(Tweet_Sparse_Test$Negative)
Baseline <- 300/(355)


#Using Random Forest to build the model
set.seed(123)
tweetRF <- randomForest(Negative ~., data = Tweet_Sparse_Train)

#Predictions
Predict_RF <- predict(tweetRF, newdata = Tweet_Sparse_Test)

#Confusion Matrix
table(Tweet_Sparse_Test$Negative, Predict_RF)

Model_Accuracy_RF <- (293+22)/(293+22+7+33)

#This is a little better than the CART model, however due to the interprebility of the
#CART model, we prefer the CART model.

#If we use Cross Validation to select the optimal cp value for the CART model, the accuracy improves
#to the same as the RF model.


#Lets use the logistic regression model to predict outcomes
Tweet_LogModel <- glm(formula = Negative ~., data = Tweet_Sparse_Train, 
                      family = "binomial")

#Prediction
Predict_LogModel <- predict(Tweet_LogModel, newdata = Tweet_Sparse_Test, 
                            type = "response")

table(Tweet_Sparse_Test$Negative, Predict_LogModel > 0.5)

Model_Accuracy_LogModel <- (251+35)/(251+35+20+49)



