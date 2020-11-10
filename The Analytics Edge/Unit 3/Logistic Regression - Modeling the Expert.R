#Logistic Regression

#Modeling the Expert
# Can we develop analytical tools that replicate expert assessment on a large scale?
# Learn from expert human judgement
# Develop a model, interpret results and adjust the model
# Make predictions/evaluations on a large scale
# Lets identify poor healthcare quality using analytics (based on claims data)



#Claims Data
#The invilve Medical Claims and Pharmacy Claims
#Not 100% accurate
#Expert Review on Sampled Claims
#Expert Assessment
#Variable Extraction: Dependent Variable - Quality of Care (binary vaiable - 1 for low and 0 for High)
#Independent Variable - Diabetes Treatment, Patient Demographics, Healthcare utilization, 
#Providers, Claims and Prescriptions


#Loading required Packages
library(tidyverse)

#Loading the data
Quality_Of_Care <- read_csv(file = "quality.csv")

#Checkout the data
summary(Quality_Of_Care)
Quality_Of_Care$PoorCare <- factor(Quality_Of_Care$PoorCare)

#Plot of Number of Narcotics Prescribed vs Number of Office Visits
Quality_Of_Care %>%
  ggplot(aes(y = Narcotics, x = OfficeVisits)) +
  geom_point(aes(colour = PoorCare), size = 2)

#What can we see?


#lets build a baseline model.
#That is predict that all patients receive good care i.e. (98/131 or 75%) percent of the time,
#patients receive good care.


#Now lets randomly split the data into a training and test set.
set.seed(88)
library(caTools)

Split_Index <- sample.split(Quality_Of_Care$PoorCare, SplitRatio = 0.75)

#Sample Split splits the data in a way that is representative of the baseline 
#probability function i.e. 0.75
Quality_Of_Care_Train <- Quality_Of_Care[Split_Index, ]

Quality_Of_Care_Test <- Quality_Of_Care[!Split_Index, ]

#Lets build our Model
Quality_Of_Care_Log_Model <- glm(formula = PoorCare ~ OfficeVisits + Narcotics, 
                                 data = Quality_Of_Care_Train, family = binomial)

#Lets look at our model
summary(Quality_Of_Care_Log_Model)


#The selection criterion for Logistics regression models is the minimum AIC


#Prediction with the logistic regression model
Predict_Train <- predict(Quality_Of_Care_Log_Model, type = "response")
summary(Predict_Train)

#How well am I predicting the Response
tapply(Predict_Train, Quality_Of_Care_Train$PoorCare, mean)



#To make a prediction, we must use a threshold value t
#t is selected based on which erros are better 
#If t is LARGE, we predict poor care rarley. This means:
#-more erroors where we say good care, but is actually poor care.
#-Detect patients receiving poor care.


#If t is small, predict good care rarely
#-More errors where we say poor care, but actually good care
#-Detext all patients receiving poor care

#With no preference between errors, use t=0.5

#We look at the confusion matrix of sensitivity and specificity

#Sensitivity = (TP)/(TP+FN) This measures the percentage of actual 
#poor cases that we classify correctly


#Specificity = (TN)/(TN+FP) This measures the percentage of actual good care 
#cases that we classify correctly

#A model with a higher threshold will have a lower sensitivity but Higher Specificity

#A model with a lower threshold will have a higher sensitivity and a lower specificity


#Confusion matrix for t = 0.5
Confusion_Matrix_1 <- table(Quality_Of_Care_Train$PoorCare, Predict_Train > 0.5)
Sensitivity <- 10/(10+15)
Specificity <- 70/(70+4)


#Confusion matrix for t = 0.7
Confusion_Matrix_2 <- table(Quality_Of_Care_Train$PoorCare, Predict_Train > 0.7)
Sensitivity <- 8/(17+8)
Specificity <- 73/(73+1)

#Confusion matrix for t = 0.2
Confusion_Matrix_3 <- table(Quality_Of_Care_Train$PoorCare, Predict_Train > 0.2)
Sensitivity <- 16/(16+9)
Specificity <- 54/(54+20)

#With a LOWER threshold, SENSITIVITY goes UP and SPECIFICITY goes DOWN.


#Now to select the right threshold value, lets use the Receiver Operator Characteristic
#curve (ROC curve)

#ROC - Curve plots (1-Specificity) on the x-axis against the True Positive rate on the y-axis
#High threshold - High Specificity, Low Sensitivity
#Low threshold - Low Specificity, High Sensitivity

#Choosing the best threshold for best trade-off
#-Cost of failing to detect positives
#-Cost of raising false claims

#Loading recquired packages
ROCR_Pred <- ROCR::prediction(Predict_Train, Quality_Of_Care_Train$PoorCare)
ROCR_Perf <- ROCR::performance(ROCR_Pred, "tpr", "fpr")
plot(ROCR_Perf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))



#Interpreting the Model
#-Check for Multi-colinearity
#-Do the coefficients make sense?


#Making Predictions
Predict_Test <- predict(Quality_Of_Care_Log_Model, type = "response", 
                        newdata = Quality_Of_Care_Test)

Confusion_Matrix_Test <- table(Quality_Of_Care_Test$PoorCare, Predict_Test > 0.3)
Sensitivity <- 6/(6+2)
Specificity <- 19/(19+5)

ROCR_Pred_Test <- ROCR::prediction(Predict_Test, Quality_Of_Care_Test$PoorCare)

AUC_Test <- as.numeric(ROCR::performance(ROCR_Pred_Test, "auc")@y.values)


