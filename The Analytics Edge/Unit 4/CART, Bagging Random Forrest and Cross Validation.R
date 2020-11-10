#Trees/ Judge, Jury, and Classifier: An introduction to Trees

#Loading required libraries
library(tidyverse)
library(caret)
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(e1071)


#Loading dataset
Stevens_Data <- read_csv(file = "stevens.csv")


#Look at the data
str(Stevens_Data)

#Sampling dataset into testing and training
set.seed(3000)
Sampling_Index <- sample.split(Stevens_Data$Reverse, SplitRatio = 0.7)

Stevens_Data_Train <- Stevens_Data[Sampling_Index, ]

Stevens_Data_Test <- Stevens_Data[!Sampling_Index, ]


#Building the regression tree: use the column names
Stevens_Tree <- rpart(formula = Reverse ~ Unconst + LowerCourt + Respondent + Petitioner +
                        Issue + Circuit, data = Stevens_Data_Train, 
                      method = "class", minbucket = 25)

#Plot the tree
prp(Stevens_Tree)

#Predictions
Predict_CART <- predict(Stevens_Tree, newdata = Stevens_Data_Test, type = "class")
table(Stevens_Data_Test$Reverse, Predict_CART)
Model_Accuracy_CART <- (41+71)/(41+36+22+71)

#ROC Curve
Predict_ROC <- predict(Stevens_Tree, newdata = Stevens_Data_Test)
Pred <- prediction(Predict_ROC[, 2], Stevens_Data_Test$Reverse)
Perf <- performance(Pred, "tpr",'fpr')
plot(Perf)


#AUC of model
as.numeric(performance(Pred, "auc")@y.values)


#The more the minbuckets, the lower the number of splits and vice versa

#Random Forrests build many CART trees
#Designed to improve prediction accuracy of CART

#Works by building a large number of CART trees but makes model less intepretable

#To make prediction for a new observation, each tree "votes" in the outcome and we 
#pick the outcome that receives the majority of votes.

#Each tree can be split on only a random subset of the variables.

#Each tree is built from a "bagged" / "bootstrapped" sample fo the data

#-Select observations randomly with replacement


#Parameters to consider
#-Minimum number of observations in a subset: controlled by the nodesize parameter
#-A smaller nodesize may take longer (computationally expensive) and leads to bigger trees


#Number of trees
#-ntree
#Should not be too small, because bagging procedure may miss observations
#more trees take longer to build
#More trees take longer to build
#Not as sensitive to parameters
#

#Loading Package
library(randomForest)


#-For random forrests, the dependent variable must be a factor variable in classification problems
Stevens_Data_Train$Reverse <- as.factor(Stevens_Data_Train$Reverse)

Stevens_Data_Test$Reverse <- as.factor(Stevens_Data_Test$Reverse)

#Building the Model
RNGkind(sample.kind = "Rounding")
set.seed(3000)
Steven_Forrest <- randomForest(formula = Reverse ~ Unconst + LowerCourt + Respondent + Petitioner +
                                 Issue + Circuit, data = Stevens_Data_Train, ntree = 200, nodesize = 25)

#Making Predicitons
Predict_Forrest <- predict(Steven_Forrest, newdata = Stevens_Data_Test)
table(Stevens_Data_Test$Reverse, Predict_Forrest)
Model_Accuracy_Forrest <- (53+72)/(53+72+24+21)

#Random Forrest is NOT sensitive to model parameters hence CV is not normally used to pick
#model parameters ntree and nodesize


#Optimal Learning

#CART Models are intepretable models - White Box

#Random Forrests are Black box and uninterpretable

#Optimal Classification trees can be used to balance between interpretability and
#prediction accuracy

#Optimal Classification Trees (OCT)
#- CART greedy training means splits are only locally optimal, overall tree could be far from
#optimal

#-Uses mordern optimization techniques to train the entire tree in one step rather than
#split-by-split training

#OCT and OCT-Hyperplanes is highly competitive with black box models

#Practicioners often have to choose between interpretability (CART) or performance (random Forrest)


#Optimal Trees  is a new method that maintains interpretability but delivers state of the 
#art performance


#Cross Validation
#In CART, the value of the "minbucket" can affect the model's out of sample accuracy

#How should we select the parameter value?

#To do this, i.e. select the right parameter value, we use the K-Fold Cross Validation


#Steps followed
#- Split training set into k pieces (or folds)
#- Use k-1 folds to estimate the model, and test model on remaining one fold (Validation set) for
#each parameter value
#Repeat for each of the k-folds
#Average the accuracy of the model over the k-folds of the parameter value


#When using cross-validation in R, we will use a parameter called cp instead,
#i.e. complexity parameter.

#Like adjusted R-squared and AIC, CP measures the trade-off between model complexity and 
#accuracy on the training set.

#A smaller cp leads to a larger tree (might overfit) and vice versa


#Selecting the right monbucket for CART
#Step 1: Selecting how many folds we want
numFolds <- trainControl(method = "cv", number = 10)

#Step 2: Pick the possible values for our cp parameter
cp_Grid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))

#Step 3: Lets perform the cross validation
train(Reverse ~ Unconst + LowerCourt + Respondent + Petitioner +
        Issue + Circuit, data = Stevens_Data_Train, method = "rpart", 
      trControl = numFolds, tuneGrid = cp_Grid)


#Now lets use the right cp for MODEL BUILDING
Steven_Tree_CV <-rpart(formula = Reverse ~ Unconst + LowerCourt + Respondent + Petitioner +
                         Issue + Circuit, data = Stevens_Data_Train, method = "class", 
                       cp = 0.19)

#Predictions using Steven_Tree_CV
Predict_CART_CV <- predict(Steven_Tree_CV, type = "class", newdata = Stevens_Data_Test)
table(Stevens_Data_Test$Reverse, Predict_CART_CV)
Model_Accuracy_CV <- (59+64)/(59+64+18+29)
#Model Accuracy of CART improves from 0.659 to 0.724

#Plotting the CART
prp(Steven_Tree_CV)

#This highlights that simple models are the BEST
