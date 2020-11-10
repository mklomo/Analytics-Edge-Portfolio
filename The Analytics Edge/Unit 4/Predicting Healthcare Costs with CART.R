#D2HawkEye: Keeping an Eye on Healthcare costs

#Dat Sources in Healthcare
#- Unstructured - Doctor's notes
#-Unavailable - hard to get due to differences in technology
#-Inaccessible - strong privacy laws around healthcare data sharing

#Types of data available
#- Claims Data - rich structured source and high dimensional data. Doesn't capture all aspects
#of persons treatment. Unlike Electronic medical records, we do not know the results of a test, only 
#that the test was administered.
#- Eligibility Information
#- Demographic information


#Variables used
#- 13,000 diagnoses transformed into 217 diagnosis groups
#- 22,000 procedures transformed into 213 procedure groups
#- 45,000 prescription drugs transformed into 189 therapeutic groups
#- Chronic condition groups
#- 269 medically defined risk rules
#- Gender and age


#Cost Variables
#Rather than using costs directly, we bucket costs and consider everyone in the group equal



#Error Measures
#R-squared
#The cost to classify High cost patient incorrectly is HIGHER than failing to classify
#a low cost patient correctly

#key idea: use asymmetric penalties
#Define a "penalty matrix" as the cost of being wrong

#Baseline is to simply predict that the cost in the next "period" will be the cost
#in the current period.

#Accuracy of 75%

#Penalty error is 0.56

#Loading required packages
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)

#Loading the data set
Claims_Data <- read_csv(file = "ClaimsData.csv")

#Looking at the data
head(Claims_Data)
str(Claims_Data)
summary(Claims_Data)


#Bucket 2009
prop.table(table(Claims_Data$bucket2008))
prop.table(table(Claims_Data$bucket2009))


#Splitting the Claims Data into training and test sets

Split_Index <- sample.split(Claims_Data$bucket2009, SplitRatio = 0.6)
Claims_Data_Train <- Claims_Data[Split_Index, ]
Claims_Data_Test <- Claims_Data[!Split_Index, ]



#Baseline Method
table(Claims_Data_Test$bucket2009, Claims_Data_Test$bucket2008)
Model_Accuracy <- (110017 + 10807 + 2783 + 1564 + 104)/ nrow(Claims_Data_Test)
PenaltyMatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), 
                        byrow = TRUE, nrow = 5)
Penalty_Error <- (sum(as.matrix(table(Claims_Data_Test$bucket2009, Claims_Data_Test$bucket2008)) *
      PenaltyMatrix))/nrow(Claims_Data_Test)

#Building the CART Model
#Cp value selected was based on CV from the Training Data
Claims_Tree <- rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd +
                       depression + diabetes + heart.failure + ihd + kidney + 
                       osteoporosis + stroke + bucket2008 + reimbursement2008, 
                     data = Claims_Data_Train, method = "class", cp = 0.00005, 
                     parms = list(loss = PenaltyMatrix))

#Take a look at the tree
prp(Claims_Tree)


Predict_Test <- predict(Claims_Tree, newdata = Claims_Data_Test, type = "class")
table(Claims_Data_Test$bucket2009, Predict_Test)
Model_Accuracy_CART <- (93756 + 19924 + 4142 + 699 +0)/nrow(Claims_Data_Test)

Penalty_Error_CART <- (sum(as.matrix(table(Claims_Data_Test$bucket2009, Predict_Test)) *
                        PenaltyMatrix))/nrow(Claims_Data_Test)


