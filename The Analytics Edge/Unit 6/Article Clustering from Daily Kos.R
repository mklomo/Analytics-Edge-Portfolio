#Document Clustering with Daily kos

#Required Packages
library(tidyverse)


#Lets load the data
Articles_Data <- read_csv(file = "dailykos.csv")


#Lets compute the distances
Articles_Distances <- dist(Articles_Data, method = "euclidean")

#Performing Hierarchical Clustering
Articles_Cluster <- hclust(Articles_Distances, method = "ward.D")

#Lets visualize
plot(Articles_Cluster)


#Lets pick 7 clusters
Articles_Cluster_Groups <- cutree(Articles_Cluster, k = 7)

#Splitting the data into the 7 clusters
Articles_Cluster_Groups_1 <- subset(Articles_Data, Articles_Cluster_Groups == 1)

Articles_Cluster_Groups_2 <- subset(Articles_Data, Articles_Cluster_Groups == 2)

Articles_Cluster_Groups_3 <- subset(Articles_Data, Articles_Cluster_Groups == 3)

Articles_Cluster_Groups_4 <- subset(Articles_Data, Articles_Cluster_Groups == 4)

Articles_Cluster_Groups_5 <- subset(Articles_Data, Articles_Cluster_Groups == 5)

Articles_Cluster_Groups_6 <- subset(Articles_Data, Articles_Cluster_Groups == 6)

Articles_Cluster_Groups_7 <- subset(Articles_Data, Articles_Cluster_Groups == 7)


#Most frequently used word in clusters
tail(sort(colMeans(Articles_Cluster_Groups_1)))

tail(sort(colMeans(Articles_Cluster_Groups_2)))

#Cluster describing the Iraq war
tail(sort(colMeans(Articles_Cluster_Groups_5)))

#Cluster Describing the DNC
tail(sort(colMeans(Articles_Cluster_Groups_7)))


#Running a k-means
RNGkind(sample.kind = "Rounding")
set.seed(1000)
Articles_KMC <- kmeans(Articles_Data, centers = 7)

#Subsets 
Articles_KMC_Cluster_1 <- subset(Articles_Data, Articles_KMC$cluster == 1)

Articles_KMC_Cluster_2 <- subset(Articles_Data, Articles_KMC$cluster == 2)

Articles_KMC_Cluster_3 <- subset(Articles_Data, Articles_KMC$cluster == 3)

Articles_KMC_Cluster_4 <- subset(Articles_Data, Articles_KMC$cluster == 4)

Articles_KMC_Cluster_5 <- subset(Articles_Data, Articles_KMC$cluster == 5)

Articles_KMC_Cluster_6 <- subset(Articles_Data, Articles_KMC$cluster == 6)

Articles_KMC_Cluster_7 <- subset(Articles_Data, Articles_KMC$cluster == 7)


#Most frequent words in wach cluster
tail(sort(colMeans(Articles_KMC_Cluster_1)))

tail(sort(colMeans(Articles_KMC_Cluster_2)))

tail(sort(colMeans(Articles_KMC_Cluster_3)))

tail(sort(colMeans(Articles_KMC_Cluster_7)))




