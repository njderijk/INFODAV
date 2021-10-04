library(igraph)
library(ggdendro)
library(dendextend)
library(ISLR)
library(tidyverse)
library(caret)
library(cowplot)

#1.
cluster <- read_csv("data/clusterdata.csv")

p1 <- ggplot(data=cluster, aes(x=x1, y=x2)) +
  geom_point() +
  coord_fixed()

p1


#2. 
c3 <- kmeans(cluster, centers = 3)
c5 <- kmeans(cluster, centers = 5)


#3. 
p2 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(c3$cluster))) +
  geom_point() +
  coord_fixed()
p2

p3 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(c5$cluster))) +
  geom_point() +
  coord_fixed()
p3


#4.
dist(cluster)
h_compl <- hclust(dist(cluster), method="complete")
h_avg <- hclust(dist(cluster), method="average")


#5.
dend_compl <- ggdendro::ggdendrogram(h_compl)
dend_compl

dend_avg <- ggdendro::ggdendrogram(h_avg)
dend_avg


#6.
list_dend <- dendlist(as.dendrogram(h_compl), as.dendrogram(h_avg))

#7.
a <- cutree(as.dendrogram(h_compl), k=3)
b <- c3[["cluster"]]

a
b

comparison <- confusionMatrix(data = as.factor(a), as.factor(b))
comparison

# When the dendrogram is cut off at three clusters, the output in terms of unique values (see tables) are very similar. This is
# proven with the help of a confusion matrix. There were only 1+1+1+3 = 6 items that were not classified the same. Out of 100,
# that is only a discrepancy of 6%. Therefore, the conclusion can be that within some error margin, the two methods yield the same
# results.


#8.
l2_dist <- function(x, y) {
  sqrt(sum((x - y) ^ 2))
}


#9.
l2_dist <- function(x, y) {
  distanceMatrix <- matrix(NA, nrow=dim(x)[1], ncol=dim(y)[1])
  for(i in 1:nrow(y)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(x)-y[i,])^2))
  }
  distanceMatrix
}

k_means <- function(matrix, centers, distanceFunction, nIterations) {
  clusterHistory <- vector(nIterations, mode="list")
  centerHistory <- vector(nIterations, mode="list")
  
  for(i in 1:nIterations) {
    distanceToCenters <- distanceFunction(matrix, centers)
    clusters <- apply(distanceToCenters, 1, which.min)
    centers <- apply(matrix, 2, tapply, clusters, mean)
    clusterHistory[[i]] <- clusters
    centerHistory[[i]] <- centers
  }
  
  list(clusters=clusterHistory, centers=centerHistory)
}

test_matrix = as.matrix(cluster) # Make sure the input is a matrix (i.e. a type that the function can process)
centers <- test_matrix[sample(nrow(test_matrix), 5),] # Determine how many 'groups' to start off with

result <- k_means(test_matrix, centers, l2_dist, 10)
result

# result[["clusters"]][[10]]

p4 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(result[["clusters"]][[10]]))) + # Take the tenth iteration
  geom_point() +
  coord_fixed() +
  theme(legend.position = "none")

p5 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(result[["clusters"]][[9]]))) + 
  geom_point() +
  coord_fixed()+
  theme(legend.position = "none")

p6 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(result[["clusters"]][[8]]))) + 
  geom_point() +
  coord_fixed()+
  theme(legend.position = "none")

p7 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(result[["clusters"]][[7]]))) + 
  geom_point() +
  coord_fixed()+
  theme(legend.position = "none")

p8 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(result[["clusters"]][[6]]))) + 
  geom_point() +
  coord_fixed()+
  theme(legend.position = "none")

p9 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(result[["clusters"]][[5]]))) + 
  geom_point() +
  coord_fixed()+
  theme(legend.position = "none")

p10 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(result[["clusters"]][[4]]))) + 
  geom_point() +
  coord_fixed()+
  theme(legend.position = "none")

p11 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(result[["clusters"]][[3]]))) + 
  geom_point() +
  coord_fixed()+
  theme(legend.position = "none")

p12 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(result[["clusters"]][[2]]))) + 
  geom_point() +
  coord_fixed()+
  theme(legend.position = "none")

p13 <- ggplot(data=cluster, aes(x=x1, y=x2, colour=as.factor(result[["clusters"]][[1]]))) +
  geom_point() +
  coord_fixed()+
  theme(legend.position = "none")

p4 # Show the visualization of the last iteration of the K_means function

plot_grid(p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, nrow=5, scale=1.5)

# Although not very clear, not a lot changes during the iterations.


