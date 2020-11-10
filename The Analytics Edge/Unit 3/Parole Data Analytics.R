#Predicting Parole Violators

#Required packages
library(tidyverse)
library(caTools)
library(ROCR)

#Loading Dataset
Parole_Data <- read_csv(file = "parole.csv")

#Looking at the data
str(Parole_Data)
summary(Parole_Data)

#Manipulating the Data for more than 2 level factor
Parole_Data$crime <- as.factor(Parole_Data$crime)
Parole_Data$state <- as.factor(Parole_Data$state)
Parole_Data$race <- ifelse(Parole_Data$race == 1,0,1)

#Violators
table(Parole_Data$violator)
table(Parole_Data$crime)
table(Parole_Data$state)


#Splitting Data into Train and Test Sets
set.seed(144)
Split_Index <- sample.split(Parole_Data$violator, SplitRatio = 0.7)

Parole_Data_Train <- Parole_Data[Split_Index, ]
Parole_Data_Test <- Parole_Data[!Split_Index, ]


#Running the Model
Parole_LogModel <- glm(formula = violator ~., family = "binomial",
                       data = Parole_Data_Train)
summary(Parole_LogModel)


#Making Prediction
Predict_Violators <- predict(Parole_LogModel, newdata = Parole_Data_Test,
                             type = "response")


#Confusion Matrix
table(Parole_Data_Test$violator, Predict_Violators > 0.5)


#Lets determine and appropriate Threshold
ROCR_Pred <- prediction(Predict_Violators, Parole_Data_Test$violator)
as.numeric(performance(ROCR_Pred, "auc")@y.values)
