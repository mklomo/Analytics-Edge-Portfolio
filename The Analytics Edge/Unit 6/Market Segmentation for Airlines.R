#Market Segmentation for Airlines

#Loading Required Libraries
library(tidyverse)
library(caret)

#Loading the dataset
Airline_Data <- read_csv(file = "AirlinesCluster.csv")

#Look at the data
summary(Airline_Data)


#Due to the different scales of the Airline Data, we will normalize that
Norm_Prep <- preProcess(Airline_Data)

Airline_Data_Norm <- predict(Norm_Prep, Airline_Data)

#Lets look at the normalized data
summary(Airline_Data_Norm)


#Computing Distances
Airline_Distance <- dist(Airline_Data_Norm, method = "euclidean")

#Hierarchical Clustering using computed distances
Airline_Clust <- hclust(Airline_Distance, method = "ward.D")

#Lets look at the dendogram
plot(Airline_Clust)


#Lets divide the airline customer data into 5 clusters
Airline_Customer_Groups <- cutree(Airline_Clust, k = 5)

#Lets check the clusters
table(Airline_Customer_Groups)


#Cluster Comparism
sort(tapply(Airline_Data$QualMiles, Airline_Customer_Groups, mean)
, decreasing = TRUE)

#Cluster 2
sort(tapply(Airline_Data$DaysSinceEnroll, Airline_Customer_Groups, mean)
     , decreasing = TRUE)

#Cluster 3
sort(tapply(Airline_Data$QualMiles, Airline_Customer_Groups, mean)
     , decreasing = TRUE)

#Cluster 4
sort(tapply(Airline_Data$BonusTrans, Airline_Customer_Groups, mean)
     , decreasing = TRUE)

#Cluster 5
sort(tapply(Airline_Data$FlightTrans, Airline_Customer_Groups, mean)
     , decreasing = TRUE)


#Using kmeans
Airlines_KMC <- kmeans(Airline_Data_Norm, 
                       centers = 5)

#Subsetting Cluster 1
Airlines_KMC_Cluster1 <- subset(Airline_Data, Airlines_KMC$cluster == 1)
  
  
#Comparing Outputs
colMeans(Airlines_KMC_Cluster1)
