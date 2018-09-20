set.seed(1234)
x <- rnorm(12, rep(1:3, each = 4), 0.2)
y <- rnorm(12, rep(c(1,2,1), each = 4), 0.2)
plot(x, y , col = "blue", pch =  19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

df <- data.frame(x = x, y = y)
dist(df) # Eucledian Distance between each point in the dataframe

rdistxy <- as.matrix(dist(df))
rdistxy
## Remove the diagonal from consideration
diag(rdistxy) <- diag(rdistxy) + 100000

# Find the index of the points with minimum distance

ind <- which(rdistxy == min(rdistxy), arr.ind = TRUE)
ind

#Create Clusters
plot(x, y , col = "blue", pch =  19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[ind[1,]], y [ind[1,]], col = "orange", pch = 19, cex = 2)

# Start Drawing the tree

par(mfrow = c(1,2))
plot(x, y , col = "blue", pch =  19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[ind[1,]], y [ind[1,]], col = "orange", pch = 19, cex = 2)

# Make a cluster and cut it at the right height

library(dplyr)
hcluster <- dist(df) %>% hclust
dendro <- as.dendrogram(hcluster)
cutDendro <- cut(dendro, h = (hcluster$height[1]))
plot(cutDendro$lower[[11]], yaxt = "n", main = "Begin building tree")

# Next Minimum

nextmin <- rdistxy[order(rdistxy)][3]
ind <- which (rdistxy == nextmin, arr.ind = TRUE)
ind

# Complete Dendogram

par(mfrow = c(1,1))
hClustering <- data.frame(x=x, y=y) %>% dist %>% hclust
plot(hClustering)


# K-means Clustering

kmeansobj <- kmeans(df, centers = 3)
names(kmeansobj)
kmeansobj$cluster
 
 