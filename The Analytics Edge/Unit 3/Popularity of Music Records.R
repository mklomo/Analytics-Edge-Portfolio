#Popularity of Music Records


#Loading required Libraries
library(tidyverse)
library(caTools)
library(ROCR)


#Loading Dataset
Songs_Data <- read_csv(file = "songs.csv")


#Obsevations in Songs Dataset
str(Songs_Data)
dim(Songs_Data)


#How many observations (songs) from 2010
Index_2010 <- Songs_Data$year >= 2010
Songs_Data_2010 <- Songs_Data[Index_2010, ]


#Michael Jackson's Songs
Songs_Data %>%
  filter(artistname == "Michael Jackson") %>%
  summarise(Number_Of_Songs = length(artistname))

Songs_Data %>%
  filter(artistname == "Michael Jackson" & Top10 == 1)

#Time Signature
Songs_Data$timesignature <- factor(Songs_Data$timesignature)
str(Songs_Data$timesignature)
summary(Songs_Data$timesignature)


#Song with the highest tempo
Songs_Data$songtitle[which.max(Songs_Data$tempo)]

#Spliting Data set into Train and Test
Subset_Index <- Songs_Data$year <= 2009
Songs_Data_Train <- Songs_Data[Subset_Index, ]
Songs_Data_Test <- Songs_Data[!Subset_Index, ]


#Number of Observations in Training Data
dim(Songs_Data_Train)


#Prep'n for Model Building by excluding non-numerical variables
non_variables<- c("year", "songtitle", "artistname", "songID", "artistID")

Songs_Data_Train <- Songs_Data_Train %>%
  select(!non_variables)

Songs_Data_Test <- Songs_Data_Test %>%
  select(!non_variables)


Songs_Data_2010 <- Songs_Data_2010 %>%
  select(!non_variables)

Songs_Data_2010$timesignature <- factor(Songs_Data_2010$timesignature)

#Training Model 1
Songs_Data_LogModel <- glm(formula = Top10 ~. , family = "binomial", 
                           data = Songs_Data_Train)

summary(Songs_Data_LogModel)

#Training Model 2
Songs_Data_LogModel_2 <- glm(formula = Top10 ~. -loudness, family = "binomial", 
                           data = Songs_Data_Train)

summary(Songs_Data_LogModel_2)



#Training Model 3
Songs_Data_LogModel_3 <- glm(formula = Top10 ~. -energy, family = "binomial", 
                             data = Songs_Data_Train)

summary(Songs_Data_LogModel_3)


#Making Predictions using Model 3
Predict_Hit <- predict(Songs_Data_LogModel_3, type = "response", 
                       newdata = Songs_Data_Test)


#Model Prediction
table(Songs_Data_Test$Top10, Predict_Hit >= 0.45)


#Baseline
table(Songs_Data_Test$Top10)






