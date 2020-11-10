#Linear Regression Techniques
#-Predicts an outcome variable, or dependent variable
#-Predicts using a set of independent techniques

#Case Study on Bordeaux Wine
#- Dependent Variable: typical price in 1990-1991 wine auctions (~ quality)

#Independent Variables
#-Age: older wines are more expensive
#-Weather: Average Growing Season Temperature, Harvest Rain and Winter Rain



#Lets start with one variable linear regression
library(tidyverse)

#loading the wine data
Wine_Train <- read_csv(file = "wine.csv")

#Take a look at the data
head(Wine_Train)

#Plot of log(Price) vs Avg Growing Season Temperature
ggplot(data = Wine_Train, aes(x = AGST, y = Price)) +
  geom_point(size = 3) +
  scale_y_continuous(trans = "log10") +
  geom_smooth(method = "lm", se = FALSE)


#The regression line minimises the sum of squarred errors.
#The Root Mean Squared Error is used as a measure of accuracy ~sqrt(SSE/N)
#Another Common Error Measure is R-squared i.e. (1-(SSE/SST)): implying that a 
#Lower SSE (which is desirable) produces a higher R-squared

#R-squared is between 0 and 1. It is unitless with a good model having a 
#high R-squared value

#Multiple Linear Regression

#First draw a scatter plot
pairs(Wine_Train)

#Now lets build a forward stepwise linear regression
library(caret)
#Note that we first model all variables individually to derrive the R-squared first.

#Model_1
AGST_Model <- lm(formula = Price ~ AGST, data = Wine_Train)
summary(AGST_Model)

#Model_2
HarvestRain_Model <- lm(formula = Price ~ HarvestRain, data = Wine_Train)
summary(HarvestRain_Model)

#Model_3
FrancePop_Model <- lm(formula = Price ~ FrancePop, data = Wine_Train)
summary(FrancePop_Model)

#Model_4
Age_Model <- lm(formula = Price ~ Age, data = Wine_Train)
summary(Age_Model)

#Model_5
WinterRain_Model <- lm(formula = Price ~ WinterRain, data = Wine_Train)
summary(WinterRain_Model)


#Now we can build a forward stepwise linear regression

#Combining Models 1 & 2
Model_6 <- lm(formula = Price ~ AGST + HarvestRain, data = Wine_Train)

#Combining Models 1, 2 and 3
Model_7 <- lm(formula = Price ~ AGST + HarvestRain + Age, data = Wine_Train)

#Combining Models 1, 2, 3 and 4
Model_8 <- lm(formula = Price ~ AGST + HarvestRain + Age + WinterRain, data = Wine_Train)

#Combining Models 1, 2, 3, 4 and 5
Model_9 <- lm(formula = Price ~ AGST + HarvestRain + Age + WinterRain + FrancePop, data = Wine_Train)

#We see that from Models 1,6, 7, 8 and 9 that adding more variables can improve the model
summary(Model_9)

#Note the diminishing improvements as more variables are added.
#The Adjusted R-squared will be crucial in determining whether an added variable 
#improves the model. Adjusted R-squared decreases when the variable added does NOT 
#improve the model


#Quick Question using Harvest Rain and Winter Rain
QQ_Model <- lm(formula = Price ~ HarvestRain + WinterRain, data = Wine_Train)
summary(QQ_Model)


#t-value = Estimate/(Std. Error)
#We want small p-values. To determine significance, please use the *** in the R-output


#The optimal model

#From Model 9, we can tell that Age and France Pop are insignificant in our model

#Lets remove France Pop because from domain knowledge of Wine production, the Age of a Wine is 
#a strong predictor of price.
Model_10 <- lm(formula = Price ~ AGST + HarvestRain + Age + WinterRain, data = Wine_Train)
summary(Model_10)

#Determining Multi-colinearity estimate using vif from the car package
car::vif(Model_10)

#We can see from the Scatterplot Matrix that Age and France Pop are highly correlated
#Hence removing the France pop variable solves the multi-colinearity problem making Age significant

cor(Wine_Train)

#Multi-collinearity is a problem caused by the high correlation between 2 independent variables
car::vif(Model_9)

#Due to incidence of Multi-collinearity, please remove insignificant variables one at a time

#Now we have determined that Model_9 is the optimal for our training data. However,
#what is the predictive power of our model for data not yet seen, new data.



#Testing Our Model's out of sample accurace
Wine_Test <- read_csv(file = "wine_test.csv")

#Take a look at the test data
head(Wine_Test)


#Predicion
Predict_Test <- predict(Model_9, newdata = Wine_Test)
SSE <- sum((Wine_Test$Price - Predict_Test)^2)
SST <- sum((Wine_Test$Price - mean(Wine_Train$Price))^2)
R-squared <- 1-SSE/SST


#When selecting a Model, we want a model with a High Tested R-squared not just a model with
#a high Train R-squard.
