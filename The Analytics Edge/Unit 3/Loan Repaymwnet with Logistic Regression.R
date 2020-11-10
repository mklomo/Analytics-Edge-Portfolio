#Loan Repayment

#Required packages
library(tidyverse)
library(caTools)
library(ROCR)

#Loading Dataset
Loans_Data <- read_csv(file = "loans.csv")


#Explore the Data
str(Loans_Data)
summary(Loans_Data)
Loans_Data$purpose <- as.factor(Loans_Data$purpose)


#Proportion of loans were not paid in full
prop.table(table(Loans_Data$not.fully.paid))

#Selecting All rows with NA
Loans_Data_NA <- Loans_Data[rowSums(is.na(Loans_Data)) > 0, ]
prop.table(table(Loans_Data_NA$not.fully.paid))


#Loans with inputed values
Loans_Data_Imputed <- read_csv(file = "loans_imputed.csv")

#Imputation from MITx course
library(mice)
set.seed(144)

var_for_imputation <- setdiff(names(Loans_Data), Loans_Data$not.fully.paid)

imputed <- complete(mice(Loans_Data[var_for_imputation]))

Loans_Data[var_for_imputation] <- imputed

#MICE Imputation uses data from the respective Independent variables to impute the missing values.
#Values from the Dependent Variable are not used.


#Making Predictions
set.seed(144)
Split_Index <- sample.split(Loans_Data_Imputed$not.fully.paid, 
                            SplitRatio = 0.7)

Loans_Data_Imputed_Train <- Loans_Data_Imputed[Split_Index, ] 

Loans_Data_Imputed_Test <- Loans_Data_Imputed[!Split_Index, ] 


#Model Building
Loans_LogModel <- glm(formula = not.fully.paid ~., data = Loans_Data_Imputed_Train,
                      family = "binomial")
summary(Loans_LogModel)


#Prediction
Predicted_Risk <- predict(Loans_LogModel, type = "response",
                          newdata = Loans_Data_Imputed_Test)

table(Loans_Data_Imputed_Test$not.fully.paid, Predicted_Risk > 0.5)



#Lets determine and appropriate Threshold
ROCR_Pred <- prediction(Predicted_Risk, Loans_Data_Imputed_Test$not.fully.paid)
as.numeric(performance(ROCR_Pred, "auc")@y.values)



#Model Building 2
Loans_LogModel_2 <- glm(formula = not.fully.paid ~ int.rate, data = Loans_Data_Imputed_Train,
                      family = "binomial")
summary(Loans_LogModel_2)


#Prediction
Predicted_Risk_2 <- predict(Loans_LogModel_2, type = "response",
                          newdata = Loans_Data_Imputed_Test)


table(Loans_Data_Imputed_Test$not.fully.paid, Predicted_Risk_2 > 0.5)

#Lets determine and appropriate Threshold
ROCR_Pred_2 <- prediction(Predicted_Risk_2, Loans_Data_Imputed_Test$not.fully.paid)
as.numeric(performance(ROCR_Pred_2, "auc")@y.values)
