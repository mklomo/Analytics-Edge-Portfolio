#Understanding why people vote

#Required Packages
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)

#Uploading the data
Voting_Data <- read_csv(file = "gerber.csv")


#Look at the data
str(Voting_Data)
head(Voting_Data)

#Proportion of voters
prop.table(table(Voting_Data$voting))

#Largest Proportion of voters
prop.table(table(Voting_Data$voting, Voting_Data$hawthorne))
prop.table(table(Voting_Data$voting, Voting_Data$civicduty))
prop.table(table(Voting_Data$voting, Voting_Data$self))
prop.table(table(Voting_Data$voting, Voting_Data$neighbors))



#Building a logistic regression
Voting_LogModel <- glm(formula = voting ~ civicduty + hawthorne + self + neighbors,
                       family = "binomial", data = Voting_Data)
summary(Voting_LogModel)


#Predictions and Model Accuracy1
Predict_LogModel <- predict(Voting_LogModel, type = "response")
table(Voting_Data$voting, Predict_LogModel > 0.3)
Model_Accuracy_LogModel_1 <- (134513 + 51966)/(134513 + 51966 + 56730 + 100875)


#Predictions and Model Accuracy 2
table(Voting_Data$voting, Predict_LogModel > 0.5)
Model_Accuracy_LogModel_2 <- (235388)/(108696+235388)


#Building AUC of the Model
ROCR_Pred_Test <- ROCR::prediction(Predict_LogModel, Voting_Data$voting)

AUC_Test <- as.numeric(ROCR::performance(ROCR_Pred_Test, "auc")@y.values)


#Building Trees
CART_Model_1 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data = Voting_Data)

#Plotting a tree
prp(CART_Model)


#Tree 2
CART_Model_2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data = Voting_Data,
                    cp = 0.0)
prp(CART_Model_2)

#Tree 3
CART_Model_3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data = Voting_Data,
                      cp = 0.0)
prp(CART_Model_3)



#Tree 4
CART_Model_4 <- rpart(voting ~ control, data = Voting_Data,
                      cp = 0.0)
prp(CART_Model_4, digits = 6)

#Tree 5
CART_Model_5 <- rpart(voting ~ control + sex, data = Voting_Data,
                      cp = 0.0)
prp(CART_Model_5)


#Back to Logistic regression
Voting_LogModel_2 <- glm(formula = voting ~ sex + control, data = Voting_Data,
                         family = "binomial")
summary(Voting_LogModel_2)


#Accounting for Control Effect
Possibilities <- data.frame(sex = c(0,0,1,1), control = c(0,1,0,1))
predict(Voting_LogModel_2, newdata = Possibilities, type = "response")



#Back to Logistic regression
Voting_LogModel_3 <- glm(formula = voting ~ sex + control + sex:control, data = Voting_Data,
                         family = "binomial")
summary(Voting_LogModel_3)

predict(Voting_LogModel_3, newdata = Possibilities, type = "response")









