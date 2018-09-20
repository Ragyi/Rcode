# Simple K-means clustering for demo

#Load dataset
library(datasets)
iris.features <- iris

#Remove Species attribute
iris.features$Species <- NULL
View(iris.features)

#Check for NAs
table(is.na(iris.features))

#Run algorithm
iris.means <- kmeans(iris.features, 3)
iris.means
iris.means$centers

#Print Results
results <- table(iris$Species, iris.means$cluster)
results

#Plot results, and compare them to original iris species
plot(iris[c("Petal.Length", "Petal.Width")], col = iris.means$cluster)
plot(iris[c("Petal.Length", "Petal.Width")], col = iris$Species)
