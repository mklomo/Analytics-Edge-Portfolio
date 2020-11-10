#Building Cluster specific prediction models


#Loading required Package
library(tidyverse)
library(caTools)
library(caret)
library(flexclust)

#Lets load the data
Stock_Data <- read_csv(file = "StocksCluster.csv")

#Lets look at the data
dim(Stock_Data)

#Proportion of observation with positive returns
prop.table(table(Stock_Data$PositiveDec))



#Correlation Matrix
max(cor(Stock_Data, method = "pearson"))

#Summary of data
summary(Stock_Data)

#Lets split the data
set.seed(144)

Split_Index <- sample.split(Stock_Data$PositiveDec, SplitRatio = 0.7)

Stocks_Data_Train <- Stock_Data[Split_Index, ]

Stocks_Data_Test <- Stock_Data[!Split_Index, ]

#Lets build a logistic regression model
Stock_LogModel <- glm(formula = PositiveDec ~., data = Stocks_Data_Train,
                      family = "binomial")

#Accuracy of the Model on the Training Set
table(Stocks_Data_Train$PositiveDec, Stock_LogModel$fitted.values > 0.5)

LogModel_Accuracy <- (990+3640)/(990+3640+2689+787)

#Test Set Predictions
Test_Prediction <- predict(Stock_LogModel, newdata = Stocks_Data_Test,
                           type = "response")

table(Stocks_Data_Test$PositiveDec, Test_Prediction > 0.5)

LogModel_Accuracy_2 <- (417+1553)/(417+1553+1160+344)

#Baseline Model
table(Stocks_Data_Test$PositiveDec)

#Cluster Stocks
limited_Train <- Stocks_Data_Train

limited_Train <- limited_Train[ , -12]

limited_Test <- Stocks_Data_Test

limited_Test <- limited_Test[ , -12]

#Important Note: In cluster-then-predict, our final goal is to predict the dependent 
#variable, which is unknown to us at the time of prediction. Therefore, if we need to 
#know the outcome value to perform the clustering, the methodology is no longer useful 
#for prediction of an unknown outcome value.
#This is an important point that is sometimes mistakenly overlooked. If you use the 
#outcome value to cluster, you might conclude your method strongly outperforms a non-clustering 
#alternative. However, this is because it is using the outcome to determine the clusters, which is not valid.

#In cases where we have a training and testing set, we'll want to normalize by the 
#mean and standard deviation of the variables in the training set.

Pre_Process_Norm <- preProcess(limited_Train)

Norm_Limited_Train <- predict(Pre_Process_Norm, limited_Train)

Norm_Limited_Test <- predict(Pre_Process_Norm, limited_Test)

#Lets cluster
set.seed(144)

KMC_NLSD <- kmeans(Norm_Limited_Train, 
                   centers = 3)

#Lets take a look at the clusters
table(KMC_NLSD$cluster)

#Clustering Stocks
km.kcca <- as.kcca(KMC_NLSD, Norm_Limited_Train)

Cluster_Train <- predict(km.kcca)

Cluster_Test <- predict(km.kcca, newdata = Norm_Limited_Test)

#Lets build Train Dataframes 
stocks_Train_1 <- subset(Stocks_Data_Train, Cluster_Train == 1)

stocks_Train_2 <- subset(Stocks_Data_Train, Cluster_Train == 2)

stocks_Train_3 <- subset(Stocks_Data_Train, Cluster_Train == 3)


#Lets build tEST Dataframes 
stock_Test_1 <- subset(Stocks_Data_Test, Cluster_Test == 1)

stock_Test_2 <- subset(Stocks_Data_Test, Cluster_Test == 2)

stock_Test_3 <- subset(Stocks_Data_Test, Cluster_Test == 3)


#LogModels
stockModel_1 <- glm(formula = PositiveDec ~., data = stock_Test_1,
                    family = "binomial")
summary(stockModel_1)

stockModel_2 <- glm(formula = PositiveDec ~., data = stock_Test_2,
                    family = "binomial")
summary(stockModel_2)

stockModel_3 <- glm(formula = PositiveDec ~., data = stock_Test_3,
                    family = "binomial")

summary(stockModel_3)


#Lets make Predictions
Predict_1 <- predict(stockModel_1, newdata = stock_Test_1,
                     type = "response")

Predict_2 <- predict(stockModel_2, newdata = stock_Test_2,
                     type = "response")

Predict_3 <- predict(stockModel_3, newdata = stock_Test_3,
                     type = "response")


table(stock_Test_1$PositiveDec, Predict_1 > 0.5)


table(stock_Test_2$PositiveDec, Predict_2 > 0.5)

table(stock_Test_3$PositiveDec, Predict_3 > 0.5)
