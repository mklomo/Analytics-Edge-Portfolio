#Climate Change

#Required Library
library(tidyverse)

#Loading the data
Climate_Change <- read_csv(file = "climate_change.csv")


#Look at the data
head(Climate_Change)
summary(Climate_Change)

#Splitting Data into Training and Testing Set
Subset_Index <- Climate_Change$Year <= 2006


Climate_Change_Train <- Climate_Change[Subset_Index,]
Climate_Change_Test <- Climate_Change[!Subset_Index, ]

#Building a regression Model to Predict Temp
Model_1 <- lm(formula = Temp ~ MEI + CO2 + CH4 + N2O + `CFC-11` + `CFC-12` + TSI + Aerosols, data = Climate_Change_Train)
summary(Model_1)


#Correlations in the dataset
cor(Climate_Change_Test[ , -c(1,2)])


#Simplifying the model
Model_2 <- lm(formula = Temp ~ MEI + TSI + Aerosols + N2O, data = Climate_Change_Train)
summary(Model_2)


#Using the AIC criterion
Model_3 <- step(Model_1)
summary(Model_3)

#Prediction
Predict_Temp <-predict(Model_3, newdata = Climate_Change_Test)
SSE <- sum((Climate_Change_Test$Temp - Predict_Temp)^2)
SST <- sum((Climate_Change_Test$Temp - mean(Climate_Change_Train$Temp))^2)
R_Squared <- 1-SSE/SST



