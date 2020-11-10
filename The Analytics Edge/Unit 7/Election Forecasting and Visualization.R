#Election Forecasting and Visualization

#Lets Load the packages
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)

#Lets get the US Map
states_map <- map_data("state")

#Lets look at the statesmap
str(states_map)

#How many groups
summary(as.factor(states_map$group))


#Lets draw a map
states_map %>%
ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

#Lets load the data
Polling <-  read_csv(file = "PollingImputed.csv")


#Lets split the data into testing and training sets
Train_Data <- subset(Polling, Year < 2012)

Test_Data <- subset(Polling, Year == 2012)


#Lets model our data
LogModel <- glm(formula = Republican ~ SurveyUSA + DiffCount, 
                data = Train_Data, family = "binomial")

#Lets perform a prediction
Test_Prediction <- predict(LogModel, newdata = Test_Data,
                           type = "response")
#Democrat vs Republican Predictions
Test_Prediction_Binary <- as.numeric(Test_Prediction > 0.5)

#Lets create a new df
prediction_Dataframe <- data.frame(Test_Prediction_Binary, Test_Prediction, Test_Data$State)


#Lets examine the new df
table(prediction_Dataframe$Test_Prediction_Binary)

#Lets merge the dataframe
prediction_Dataframe$region <- tolower(prediction_Dataframe$Test_Data.State)

prediction_Map <- merge(states_map, prediction_Dataframe, by = "region")

#Lets order the map
prediction_Map <- prediction_Map[order(prediction_Map$order), ]

#Lets color the map
prediction_Map %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Test_Prediction)) +
  geom_polygon(color = "black", linetype = 2, size = 3) +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0,1), 
                    labels = c("Democrat", "Republican"), name = "Prediction 2012")

#Lets color the map
prediction_Map %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Test_Prediction)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0,1), 
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")





