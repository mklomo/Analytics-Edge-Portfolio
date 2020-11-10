#Recommendations for movies and health - Clustering

#What data could be used to predict user ratings?
#- Movie ranking from all users who have ranked the movie
#- Facts about the movie itself: actors, director, genre classifications, year released, etc


#Collaborative Filtering: using other user's ratings to make predictions. This is
#done using similarity between users and no information about the movie itself.
#Here, we can accurately predict complex items without understanding the  nature of the items.
#Requires a lot of data about the user to make accurate recommendations- hence commputationally expensive.


#Content Filtering: this is where we use the movie information to predict movies for
#the user. Here, we do not use the predictions of other users.
#Requires very litlle data to get started and can be limited in scope.


#Netflix uses both content and collaborative filtering i.e. hybrid approach.



#Clustering and Content Filtering using Clustering.
#Data - Movies are categorized as belonging todifferent genres as well as an unknown category
#Can we systematically find groups find groups of movies with simlar sets of genres?
#We use clustering to answer this question. Clustering is an unsupervised learning method.
#- The goal is to segment the data into similar groupsinstead of prediction.
#- Clustering can be used to improve predictive methods. 
#- We can cluster the data into similar groups and then build a predictive model for each group.
#- Watchout for over-fotting when doing this.



#- Algorithms for clustering
#- Hierarchical Clustering & K-means clustering.


#How does clustering work?
#- Need to define distance between the 2 data points.
#- We mostly use the "Euclidean Distance"
#- We can also use the "Manhattan Distance" or "Maximum Coordinate Distance"
#We also need to calculate the distance between clusters. We can use "Minimum Distance
#or "Maximum Distance" or "Centroid Distance"

#Points to note
#- Distance is highly influenced by the scale of the variable. Hence, its necessary 
#to NORMALIZE the data first.



#Hierarchical Clustering
#At the endo of a hierarchical clustering process, all our data points are 
#in a SINGLE cluster
# A Dendogram displays the clusters and the relationships between them. The height 
#of the lines shows how far apart the clusters were when combined.

#We can use a dendogram to decide how many clusters we want for our final 
#clustering model.

#After selecting the nuber of clusters, determine whether the clusters are meaningful
#by looking at basic statistics in each cluster like the mean, minimum, and maximum values.
#Also see if the clusters have a feature in common that was not used in the clustering (like an outcome).2

#Loading required Packages
library(tidyverse)

#Loading the data set
Movie_Data <- read.table(file = "http://files.grouplens.org/datasets/movielens/ml-100k/u.item", sep = "|",
                         header = FALSE, quote = "\"")

Movie_Data <- as_tibble(Movie_Data)


colnames(Movie_Data) <- c("ID", "Title", "Release_Date", "Video_Release_Date", "IMDB",
                          "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy",
                          "Crime", "Documentary", "Drama", "Fantasy", "Film_Noir", "Horror",
                          "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

#Removing selected Clumns
Col_to_Remove <- c("ID", "Release_Date", "Video_Release_Date", "IMDB")
Movie_Data <- Movie_Data %>%
  select(!Col_to_Remove)


#Keeping only unique observations
Movie_Data <- unique(Movie_Data)







#Using hierarchical clustering
#- First compute the distances
str(Movie_Data)
Distances_Hclus <- dist(Movie_Data[ , 2:20], method = "euclidean")
cluster_Movies <- hclust(Distances_Hclus, method = "ward.D")
plot(cluster_Movies)

#To pick the right number of clusters, we need to use our understanding of the problem
#and the dendogram.
cluster_group <- cutree(cluster_Movies, k = 10)
tapply(Movie_Data$Action, cluster_group, mean)
tapply(Movie_Data$Romance, cluster_group, mean)

#Lets perform content filtering for a person who loved the movie "Men in Black (1997)
MIB_index <- Movie_Data$Title %in% c("Men in Black (1997)")


#Which cluster does Men in Black belong?
cluster_group[MIB_index]
#Men in Black belong to cluster 2

#Movies is Cluster 2
cluster_2 <- subset(Movie_Data, cluster_group == 2)

#Content filtering recommendations
cluster_2$Title

