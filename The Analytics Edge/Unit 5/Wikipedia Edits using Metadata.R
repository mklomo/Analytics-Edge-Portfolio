#Wikipedia Edits

#Load required library
library(tidyverse)
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)

#Loading the dataset
Wiki_Data <- read_csv(file = "wiki.csv")

#Lets take a look
str(Wiki_Data)

#Cases of Vandalism
table(Wiki_Data$Vandal)


#Creating Corpus for Column Added
Added_Corpus <- Corpus(VectorSource(Wiki_Data$Added))


#Removing English Language stop words
Added_Corpus <- tm_map(Added_Corpus, removeWords, stopwords("english"))


#Stemming the words
Added_Corpus <- tm_map(Added_Corpus, stemDocument)


#Document Term Matrix
dtm_Added <- DocumentTermMatrix(Added_Corpus)


#Keeping only key words that appear in 0.3% 
sparse_Added <- removeSparseTerms(dtm_Added, sparse = 0.997)


#Converting sparse added to a datafram
sparse_Added <- as_tibble(as.matrix(sparse_Added))

colnames(sparse_Added) <- make.names(colnames(sparse_Added))

#Renaming dataframe
words_Added <- sparse_Added

#Prepending all words added with the letter A
colnames(words_Added) <- paste("A", colnames(words_Added))


#Repeating the same for words removed
#Creating Corpus for Column Removed
Removed_Corpus <- Corpus(VectorSource(Wiki_Data$Removed))


#Removing English Language stop words
Removed_Corpus <- tm_map(Removed_Corpus, removeWords, stopwords("english"))


#Stemming the words
Removed_Corpus <- tm_map(Removed_Corpus, stemDocument)


#Document Term Matrix
dtm_Removed <- DocumentTermMatrix(Removed_Corpus)


#Keeping only key words that appear in 0.3% 
sparse_Removed <- removeSparseTerms(dtm_Removed, sparse = 0.997)

#Converting sparse added to a datafram
sparse_Removed <- as_tibble(as.matrix(sparse_Removed))

colnames(sparse_Removed) <- make.names(colnames(sparse_Removed))

#Renaming dataframe
words_Removed <- sparse_Removed

#Prepending all words added with the letter A
colnames(words_Removed) <- paste("R", colnames(words_Removed))

#Combining the 2 tibbles
wiki_Words <- cbind(words_Added, words_Removed)

#Adding Dependent Variable
wiki_Words$Vandal <- Wiki_Data$Vandal

#Lets predict
set.seed(123)


#Spliting the dataset
Split_Index <- sample.split(wiki_Words$Vandal, SplitRatio = 0.7)

#Test and Training Set
wiki_Words_Train <- wiki_Words[Split_Index, ]
wiki_Words_Test <- wiki_Words[!Split_Index, ]


#Baseline Accuracy
table(wiki_Words_Test$Vandal)


#Building a CART model
Vandal_CART_Model <- rpart(formula = Vandal ~., data = wiki_Words_Train, 
                           method = "class")

#Prediction
Predict_CART <- predict(Vandal_CART_Model, newdata = wiki_Words_Test)

#Model Accuracy
table(wiki_Words_Test$Vandal, Predict_CART[,2])

Model_Accuracy_CART <- (614+19)/(614+19+4+526)


#Tree
prp(Vandal_CART_Model)

#Using website data
 
wiki_Words2 = wiki_Words

#Make a new column in wikiWords2 that is 1 if "http" was in Added:
  
wiki_Words2$HTTP = ifelse(grepl("http",Wiki_Data$Added,fixed=TRUE), 1, 0)

#Based on http, how many revisions added a link
table(wiki_Words2$HTTP)

#Spliting wikiwords2 into traning and test sets
wiki_Words2_Train <- wiki_Words2[Split_Index, ]

wiki_Words2_Test <- wiki_Words2[!Split_Index, ]


#Building a CART Model
#Building a CART model
Vandal_CART_Model2 <- rpart(formula = Vandal ~., data = wiki_Words2_Train, 
                           method = "class")

#Prediction
Predict_CART_2 <- predict(Vandal_CART_Model2, newdata = wiki_Words2_Test)

#Model Accuracy
table(wiki_Words2_Test$Vandal, Predict_CART_2[,2])

Model_Accuracy_CART_2 <- (605+45+19)/(605+45+19+13+481)

#Trial 3: Another possibility is that the number of words added and removed is predictive, perhaps more so than the actual words themselves.

wiki_Words2$NumWordsAdded <- rowSums(as.matrix(dtm_Added))

wiki_Words2$NumWordsRemoved <- rowSums(as.matrix(dtm_Removed))



#Average words added
mean(wiki_Words2$NumWordsAdded)


#Respliting wikiwords 2
wiki_Words2_Train <- wiki_Words2[Split_Index, ]

wiki_Words2_Test <- wiki_Words2[!Split_Index, ]


#Building a CART model
Vandal_CART_Model3 <- rpart(formula = Vandal ~., data = wiki_Words2_Train, 
                            method = "class")

#Prediction
Predict_CART_3 <- predict(Vandal_CART_Model3, newdata = wiki_Words2_Test)

#Model Accuracy
table(wiki_Words2_Test$Vandal, Predict_CART_3[,2])

Model_Accuracy_CART_3 <- (330+184+242+6)/(330+184+242+6+104+0+144+153)


#Trial 4
wiki_Words3 <- wiki_Words2

#Adding original variables
wiki_Words3$Minor <- Wiki_Data$Minor

wiki_Words3$LoggedIn <- Wiki_Data$Loggedin


#Resplitting the data
wiki_Words3_Train <- wiki_Words3[Split_Index, ]

wiki_Words3_Test <- wiki_Words3[!Split_Index, ]

#Building a CART model
Vandal_CART_Model4 <- rpart(formula = Vandal ~., data = wiki_Words3_Train, 
                            method = "class")

#Prediction
Predict_CART_4 <- predict(Vandal_CART_Model4, newdata = wiki_Words3_Test)

#Model Accuracy
table(wiki_Words3_Test$Vandal, Predict_CART_4[,2])

Model_Accuracy_CART_4 <- (539+56+7+234)/(539+56+7+234+1+22+253+51)


#Plot the data
prp(Vandal_CART_Model4)



