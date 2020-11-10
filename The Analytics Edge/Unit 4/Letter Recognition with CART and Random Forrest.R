#Letter recognition

#Required Libraries
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(randomForest)


#Loading the data
LR_Data <- read_csv(file = "letters_ABPR.csv")


#Creating a new Variable
LR_Data <- LR_Data %>%
  mutate(isB = as.factor(letter == "B"))


#Splitting Dataset
set.seed(1000)
Split_Index <- sample.split(LR_Data$isB, SplitRatio = 0.5)

LR_Data_isB_Train <- LR_Data[Split_Index, ]  
LR_Data_isB_Test <- LR_Data[!Split_Index, ]  


#Baseline
prop.table(table(LR_Data_isB_Test$isB))
  

#Building the model to predict B
CARTb <- rpart(isB ~. -letter, data = LR_Data_isB_Train, method = "class")

#Making Predictions
Predict_CARTb <- predict(CARTb, type = "class", newdata = LR_Data_isB_Test)

table(LR_Data_isB_Test$isB, Predict_CARTb)  


#Building a Random Forrest Model
set.seed(1000)
isB_RandomForrest <- randomForest(isB ~. -letter, data = LR_Data_isB_Train, 
                                  importance = TRUE)

#Prediction with RF
Prediction_RFb <- predict(isB_RandomForrest, newdata = LR_Data_isB_Test)
table(LR_Data_isB_Test$isB, Prediction_RFb)


#Multiclass prediction with RF
LR_Data$letter <- as.factor(LR_Data$letter)

#Generating New Test and Train Sets
RNGkind(sample.kind = "Rounding")
set.seed(2000)
Split_Index_2 <- sample.split(LR_Data$letter, SplitRatio = 0.5)
LR_Data_Train <- LR_Data[Split_Index_2, ]
LR_Data_Test <- LR_Data[!Split_Index_2, ]


#Baseline of the test set
prop.table(table(LR_Data_Test$letter))


#Building CART Model with Training data
Letter_CART <- rpart(letter ~. -isB, method = "class", data = LR_Data_Train)

#Prediction
Predict_Letter_CART <- predict(Letter_CART, newdata = LR_Data_Test, 
                               type = "class")
#Confusion Matrix
table(LR_Data_Test$letter, Predict_Letter_CART)


#Random Forrest Prediction
set.seed(1000)
letter_RF <- randomForest(letter ~. -isB, data = LR_Data_Train, importance= TRUE)

#Making Predictions
Predict_Letter_RF <-predict(letter_RF, newdata = LR_Data_Test)

table(LR_Data_Test$letter, Predict_Letter_RF)

Model_Accuracy_RF <- (389+379+393+365)/nrow(LR_Data_Test)

