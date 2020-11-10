#Visualizing  Network Data


#Loading the data
library(tidyverse)
library(igraph)

Edges <- read_csv(file = "edges.csv")

Users <- read_csv(file = "users.csv")

#Lets plot the graph
g <- graph.data.frame(Edges,FALSE, Users)

plot(g, vertex.size = 5, vertex.label = NA)


#Lets find the degree of a node
Net_Degree <- as.data.frame(degree(g))
Net_Degree %>%
  arrange(desc(degree(g)))


#Highlighting users with higher degrees
V(g)$size = degree(g)/2+2

plot(g, vertex.label = NA)

#More specification - Gender
V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label = NA)


#More specification - School
V(g)$color <- "black"

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "AB"] = "blue"

plot(g, vertex.label = NA)

#More specification - Vertices
V(g)$color <- "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "blue"

plot(g, vertex.label = NA, vertex.shape = "square")

