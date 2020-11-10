#Reading Test Scores


#Required libraries
library(tidyverse)

#Loading the Dataset
Pisa_Train <- read_csv(file = "pisa2009train.csv")
Pisa_Test <- read_csv(file = "pisa2009test.csv")


#Dimensions of dataset
dim(Pisa_Train)


#Summarizing the Dataset
Pisa_Train %>%
  group_by(male) %>%
  summarise(Ave_Read_Score = mean(readingScore))

#Look at NA's
Pisa_Train$raceeth <- factor(Pisa_Train$raceeth)
Pisa_Test$raceeth <- factor(Pisa_Test$raceeth)
summary(Pisa_Train)


#Removing NA's
Pisa_Train <- na.omit(Pisa_Train)
Pisa_Test <- na.omit(Pisa_Test)

#Re_ordering the racceeth variable
Pisa_Train$raceeth <- relevel(Pisa_Train$raceeth, "White")
Pisa_Test$raceeth <- relevel(Pisa_Test$raceeth, "White")

#Building Reg Model
lm_Score <- lm(formula = readingScore ~. , data = Pisa_Train)
summary(lm_Score)

#RMSE
SSE <- sum((Pisa_Train$readingScore - lm_Score$fitted.values)^2)
RMSE <- sqrt(SSE/nrow(Pisa_Train))


#Predicting Test
Pred_Test <- predict(lm_Score, newdata = Pisa_Test)
summary(Pred_Test)

SSE <- sum((Pisa_Test$readingScore - Pred_Test)^2)
SST <- sum((Pisa_Test$readingScore - mean(Pisa_Train$readingScore))^2)
1-SSE/SST

RMSE <- sqrt(SSE/nrow(Pisa_Test))
