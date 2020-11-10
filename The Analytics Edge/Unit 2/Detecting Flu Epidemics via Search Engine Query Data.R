#Detecting Flu Pandemics via search engine querry data

#Loading required libraries
library(tidyverse)

#Loading Dataset
Flu_data_Train <- read_csv(file = "FluTrain.csv")

#Look at the data
head(Flu_data_Train)

#Week with high reported ILI
index <- which.max(Flu_data_Train$ILI)

#Week with highest ILI
Flu_data_Train$Week[index]


#Week with highest ILI-related query fraction
Flu_data_Train$Week[which.max(Flu_data_Train$Queries)]


#Histogram of ILI
Flu_data_Train %>%
  ggplot(aes(x = ILI)) +
  geom_histogram(binwidth = 0.25)


#Plot of ILI against Queries
Flu_data_Train %>%
  ggplot(aes(y = log(ILI), x = Queries)) +
  geom_point()

#We use a log function here because the ILI variable are very small values with 
#some large tails. The natural log prevents the data from being skewed by the maximum
#values.



#Building the model
Flu_Trend_Reg_Model <- lm(formula = log(ILI) ~ Queries, data = Flu_data_Train)
summary(Flu_Trend_Reg_Model)


#Flu Test
Flu_data_Test <- read_csv(file = "FluTest.csv")

#Load the data
head(Flu_data_Test)


#Prediction
Predict_ILI <- exp(predict(Flu_Trend_Reg_Model, newdata = Flu_data_Test))

#Finding the Prediciton for March 11
index <- which(Flu_data_Test == "2012-03-11 - 2012-03-17")

#Prediction for March 11 is:
Predict_ILI[index]

#Prediction Error
(Flu_data_Test$ILI[index] - Predict_ILI[index])/(Flu_data_Test$ILI[index])
  
  
#RMSE
SSE <- sum((Flu_data_Test$ILI - Predict_ILI)^2)
SST <- sum((Flu_data_Test$ILI - mean(Flu_data_Train$ILI))^2)  

RMSE <-sqrt(SSE/nrow(Flu_data_Test))
  

#Intro to Time Series Models
library(zoo)

x <- zoo(Flu_data_Train$ILI) 


#To use lag from stats and not from the dplur package please specify the stats package
ILILag_2 <- stats::lag(x, -2, na.pad = TRUE)

Flu_data_Train$ILILag_2 <- coredata(ILILag_2)

#plot
Flu_data_Train %>%
  ggplot(aes(x = ILILag_2, y = ILI)) +
  geom_point()


#Traing the Reg_Model
Flu_Trend_Reg_Model_2 <- lm(formula = log(ILI) ~ Queries + log(ILILag_2), data = Flu_data_Train)
summary(Flu_Trend_Reg_Model_2)

#Repeating same for Test Data i.e. adding time series component
library(zoo)

y <- zoo(Flu_data_Test$ILI) 


#To use lag from stats and not from the dplur package please specify the stats package
ILILag_2 <- stats::lag(y, -2, na.pad = TRUE)

Flu_data_Test$ILILag_2 <- coredata(ILILag_2)


#Since the data is sequential, lets fill out the missing values
#Filling out the first NA
Flu_data_Test$ILILag_2[1] <- Flu_data_Train$ILI[416]

#Filling out the Second NA
Flu_data_Test$ILILag_2[2] <- Flu_data_Train$ILI[417]


#Prediction of ILI with added time series component
Predict_ILI_2 <- exp(predict(Flu_Trend_Reg_Model_2, newdata = Flu_data_Test))

#RMSE
SSE <- sum((Flu_data_Test$ILI - Predict_ILI_2)^2)
SST <- sum((Flu_data_Test$ILI - mean(Flu_data_Train$ILI))^2)  

RMSE <-sqrt(SSE/nrow(Flu_data_Test))



