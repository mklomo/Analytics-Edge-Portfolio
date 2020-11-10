#Clustering of Greyscale brain images data: Medical Imaging Applications

#Using k-means clustering
#1 - Specify desired number of clusters
#2 - Randomly assign each data point to a cluster
#3 - Compute cluster centroids
#4 - Re-assign each point to the closest cluster centroid
#5 - Re-compute cluster centroids
#6 - Repeat 4 and 5 until no improvement is made


#Practical considerations
#- The number of clusters k can be selected from previous knowledge
#- Can strategically initial partition of points into clusters if you have 
#some knowledge of teh data
#- Can run algorithm several times with different random points

#A major difference between k-means clustering and Hierarchical clustering is that
#with k-means, you pick the number of clusters before running the algorithm

#Goal: Partition image to clusters based on differences in pixel colors, intensity or texture
#Image is represented as a matrix of pixel intensity values ranging from 0 to 1
#For 8bits/pixel (bpp), 256 color levels

#Required Packages
library(tidyverse)
library(flexclust)


#Loading the dataset: 50 by 50 grey scale flower picture
Flower_Data <- read_csv(file = "flower.csv", col_names = FALSE)

#Changing the data to a matrix 
Flower_Matrix <- as.matrix(Flower_Data)

#Then change to vector
Flower_Vector <- as.vector(Flower_Matrix)

#Lets perform the hierarchical clustering
#Computing pairwise distances
distance <- dist(Flower_Vector, method = "euclidean")

cluster_Intensity <- hclust(distance, method = "ward.D")

#Plot the cluster
plot(cluster_Intensity)

rect.hclust(cluster_Intensity, k = 3, border = "red")

flower_Clusters <- cutree(cluster_Intensity,k = 3)

tapply(Flower_Vector, flower_Clusters, mean)

dim(flower_Clusters) <- c (50, 50)
image(flower_Clusters, axes = FALSE)
image(Flower_Matrix, axes = FALSE, col = grey(seq(0,1,length = 256)))


#MRI Image
Healthy_Data <- read_csv(file = "healthy.csv", col_names = FALSE)

#Lets convert the MRI 
Healthy_Matrix <- as.matrix(Healthy_Data)

#Lets take a look at the image
image(Healthy_Matrix, axes = FALSE, col = grey(seq(0,1,length = 256)))


#Lets vectorize the data now
Healthy_Vector <- as.vector(Healthy_Matrix)

#Lets compute the hierarchical distances
distance <- dist(Healthy_Vector, method = "euclidean")

#There is a problem with the vector memory
#Due to the large size of the vector matrixes to be computed, we can not use
#hierarchical clustering.

#Using K-means Clustering
k = 5

set.seed(1)

KMC <- kmeans(Healthy_Vector, centers = k, 
              iter.max = 1000)
str(KMC)

Healthy_Cluster <- KMC$cluster
KMC$centers[1]

dim(Healthy_Cluster) <- c(nrow(Healthy_Matrix), ncol(Healthy_Matrix))

image(Healthy_Cluster, axes = FALSE, col = rainbow(k))


#Using a Screeplot to determine the number of clusters
#based on the within cluster sum of squares



#Can we use the clusters to detect a tumor from an MRI?

Tumor_Data <- read_csv(file = "tumor.csv", col_names = FALSE)
Tumor_Matrix <- as.matrix(Tumor_Data)
Tumor_Vector <- as.vector(Tumor_Matrix)

#Traning on Object
KCM.kcca <- as.kcca(KMC, Healthy_Vector)

#Prediction
tumor_Clusters <- predict(KCM.kcca, newdata = Tumor_Vector)

dim(tumor_Clusters) <- c(nrow(Tumor_Matrix), ncol(Tumor_Matrix))

#Lets plot the image
image(tumor_Clusters, axes = FALSE, col = rainbow(5))

