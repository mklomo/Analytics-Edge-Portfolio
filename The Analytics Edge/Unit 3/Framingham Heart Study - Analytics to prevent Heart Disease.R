#Framingham Heart Study: Analytics to prevent Heart Disease


#Predicting the 10 year risk of Coronary Heart Disease (CHD)


#Hypothesis: Risk factors identified in the study: 
#- Demographic Factors: male, age, education
#- Behavorial risk factors: CurrentSmoker, CigsPerDay, SmokingBehavior
#- Medical History risk factors: BPmeds, PrevalentSmoke, PrevalentHyp, Diabetes
#- First Examinations: totChol, sysBP, diaBP, BMI, heartRate, glucose

#Required Libraries
library(tidyverse)
library(ROCR)
library(caTools)

#Loading Required Data
CHD_Data <- read_csv(file = "framingham.csv")


#Take a look at the Data
head(CHD_Data)
summary(CHD_Data)
str(CHD_Data)

#Removing NA's
CHD_Data <- na.omit(CHD_Data)



#Splitting Data into Training and Test Data
set.seed(1000)
Train_Index <- sample.split(CHD_Data$TenYearCHD, SplitRatio = 0.65)
CHD_Data_Train <- CHD_Data[Train_Index, ]
CHD_Data_Test <- CHD_Data[!Train_Index, ]


#Building the Logistics Regression Model
CHD_Log <- glm( formula = TenYearCHD ~. , data = CHD_Data_Train, family = binomial)
summary(CHD_Log)


#Prediction using test set
Predict_Test <- predict(CHD_Log, newdata = CHD_Data_Test, type = "response")

#Confusion Matrix
table(CHD_Data_Test$TenYearCHD, Predict_Test > 0.5)

#Model Accuracy
(1081+18)/(1081+18+4+177)

#Baseline Model
(1081+4)/(1081+18+4+177)


#Lets determine and appropriate Threshold
ROCR_Pred <- prediction(Predict_Test, CHD_Data_Test$TenYearCHD)
as.numeric(performance(ROCR_Pred, "auc")@y.values)
#Out of Scaple AUC is 0.71



#Or You can work with all the NAs and onely omit at the prediction stage which is
#probably the better option as we work with more data

#Loading Required Data
CHD_Data <- read_csv(file = "framingham.csv")



#Splitting Data into Training and Test Data
set.seed(1000)
Train_Index <- sample.split(CHD_Data$TenYearCHD, SplitRatio = 0.65)
CHD_Data_Train <- CHD_Data[Train_Index, ]
CHD_Data_Test <- CHD_Data[!Train_Index, ]


#Building the Logistics Regression Model
CHD_Log <- glm( formula = TenYearCHD ~. , data = CHD_Data_Train, family = binomial)
summary(CHD_Log)


#Prediction using test set
Predict_Test <- predict(CHD_Log, newdata = CHD_Data_Test, type = "response")

#Confusion Matrix
table(CHD_Data_Test$TenYearCHD, Predict_Test > 0.5)

#Model Accuracy
(1069+11)/(1069+11+6+187)

#Baseline Model
(1069+6)/(1069+11+6+187)


#Lets determine and appropriate Threshold
df_clean <- na.omit(tibble(Predict_Test, Test_TenYearCHD = CHD_Data_Test$TenYearCHD))
ROCR_Pred <- prediction(df_clean$Predict_Test, df_clean$Test_TenYearCHD)
as.numeric(performance(ROCR_Pred, "auc")@y.values)

#The Out of Sample AUC = 0.74


#Risk Model Validation: For models that will be used on different populations not
#included in the original group of patients, the model can be recalibrated for the 
#new population. Recalibration scales down the predictors.
