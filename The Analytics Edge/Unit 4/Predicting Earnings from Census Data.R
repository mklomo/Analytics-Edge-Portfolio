#Predicting Earnings from Census Data

#Packages needed
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)
library(randomForest)

#Loading the data
Census_Data <- read_csv(file = "census.csv")


#Looking at the data
str(Census_Data)
summary(Census_Data)

#Converting the over50k into a factor
Census_Data$over50k <- as.factor(Census_Data$over50k)
Census_Data$workclass <- as.factor(Census_Data$workclass)
Census_Data$education <- as.factor(Census_Data$education)
Census_Data$maritalstatus <- as.factor(Census_Data$maritalstatus)
Census_Data$occupation <- as.factor(Census_Data$occupation)
Census_Data$relationship <- as.factor(Census_Data$relationship)
Census_Data$race <- as.factor(Census_Data$race)
Census_Data$sex <- as.factor(Census_Data$sex)
Census_Data$nativecountry <- as.factor(Census_Data$nativecountry)


#Training and Testing sets
set.seed(2000)
Split_Index <- sample.split(Census_Data$over50k, SplitRatio = 0.6)
Census_Data_Train <- Census_Data[Split_Index, ]
Census_Data_Test <- Census_Data[!Split_Index, ]


#Building a logistics Regression Model
Census_Data_LogModel <- glm(formula = over50k ~. , family = "binomial", 
                            data = Census_Data_Train)
summary(Census_Data_LogModel)


#Prediction
Predict_LogModel <- predict(Census_Data_LogModel, type = "response", 
                            newdata = Census_Data_Test)

table(Census_Data_Test$over50k, Predict_LogModel > 0.5)

Model_Accuracy_LogModel <- (9051+1888)/(9051+1888+1190+662)

#Baseline for test set
prop.table(table(Census_Data_Test$over50k))


#AUC of the model
ROCR_Pred_Test <- prediction(Predict_LogModel, Census_Data_Test$over50k)

AUC_Test <- as.numeric(performance(ROCR_Pred_Test, "auc")@y.values)


#Building a CART Tree
Census_CART_Model <- rpart(over50k ~. , method = "class", 
                           data = Census_Data_Train)

prp(Census_CART_Model)


#Accuracy on the Testing Set
Predict_CART <- predict(Census_CART_Model, type = "class", 
                        newdata = Census_Data_Test)
table(Census_Data_Test$over50k, Predict_CART)

Model_Accuracy_CART <- (9243+1596)/(9243+1596+470+1482)


#ROC Curve for CART
Predict_CART_ROC <- predict(Census_CART_Model, newdata = Census_Data_Test)

#Loading recquired packages
CART_ROCR_Pred <- prediction(Predict_CART_ROC[,2], Census_Data_Test$over50k)
CART_ROCR_Perf <- performance(CART_ROCR_Pred, "tpr", "fpr")
plot(CART_ROCR_Perf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), 
     text.adj = c(-0.2,1.7))

AUC_Test <- as.numeric(performance(CART_ROCR_Pred, "auc")@y.values)


#Building a Random Forrest

#Sampled down dataset
set.seed(1)
trainSmall <- Census_Data_Train[sample(nrow(Census_Data_Train), 2000), ]

Census_Data_RFmodel <- randomForest(over50k ~., data = Census_Data_Train)

#Prediction
RF_Model_Prediction <- predict(Census_Data_RFmodel, newdata = Census_Data_Test) 

table(Census_Data_Test$over50k, RF_Model_Prediction)

RF_Model_Accuracy <- (9021+2049)/nrow(Census_Data_Test)


#Important variables in a RF 1
vu <- varUsed(Census_Data_RFmodel, count = TRUE)

vuSorted <- sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vuSorted$x, names(Census_Data_RFmodel$forest$xlevels[vuSorted$ix]))


#Important variables in a RF 2
varImpPlot(Census_Data_RFmodel)


#Selecting cp by Cross Validation
set.seed(2)
#Selecting the right monbucket for CART
#Step 1: Selecting how many folds we want
numFolds <- trainControl(method = "cv", number = 10)

#Step 2: Pick the possible values for our cp parameter
CART_Grid <- expand.grid(.cp = seq(0.002, 0.1, 0.002))

#Step 3: Lets perform the cross validation
train(over50k ~., data = Census_Data_Train, method = "rpart", 
      trControl = numFolds, tuneGrid = CART_Grid)

#Fitting a CART Model
Census_CART_Model_2 <- rpart(over50k ~., data = Census_Data_Train, 
                             method = "class", cp = 0.002)

#Prediction
CART_Prediction_2 <- predict(Census_CART_Model_2, type = "class", 
                             newdata = Census_Data_Test)

table(Census_Data_Test$over50k, CART_Prediction_2)

CART2_Model_Accuracy <- (9178+1838)/(9178+1838+1240+535)

#Plotting the CART Model
prp(Census_CART_Model_2)








