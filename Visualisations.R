#Histograms

library(RColorBrewer)
data(VADeaths)
par(mfrow=c(2,3))
hist(VADeaths, breaks = 10, col= brewer.pal(3, "Set3"), main= "Set3 3 Colors")
hist(VADeaths, breaks = 3, col= brewer.pal(3, "Set2"), main = "Set2 3 Colors")
hist(VADeaths, breaks = 7, col = brewer.pal(3, "Set1"), main = "Set1 3 Colors")
hist(VADeaths, breaks = 2, col = brewer.pal(8, "Set3"), main = "Set3 8 Colors")
hist(VADeaths, col = brewer.pal(8, "Greys"), main = "Greys 8 Colors")
hist(VADeaths, col = brewer.pal(8, "Greens"), main = "Greens 8 Colors")


#Line Charts
par(mfrow=c(1,1))
plot(AirPassengers, type = "l")


#BarCharts

barplot(iris$Petal.Length)
barplot(iris$Sepal.Length, col = brewer.pal(3, "Set1"))
barplot(table(iris$Species, iris$Sepal.Length), col = brewer.pal(3, "Set1"))


#Boxplot

data(iris)
par(mfrow= c(1,1))
boxplot(iris$Sepal.Length, col= "red")
boxplot(iris$Sepal.Length ~ iris$Species, col= "red")
boxplot(iris$Sepal.Length ~ iris$Species, col= heat.colors(3))
boxplot(iris$Sepal.Length ~ iris$Species, col= topo.colors(3))


#Scatter Plots

plot(x = iris$Petal.Length)
plot(x = iris$Petal.Length, y= iris$Species)

plot(iris, col = brewer.pal(3, "Set1"))

pie(table(iris$Species))


#Advanced Visulisations

#Basic Hexbin
install.packages("hexbin")
library(hexbin)
library(ggplot2)
library(RColorBrewer)

a = hexbin(diamonds$price, diamonds$carat, xbins = 40)
plot(a)

#Color Hexbin

rf <- colorRampPalette(rev(brewer.pal(40, "Set3")))
hexbinplot(diamonds$price ~ diamonds$carat, data= diamonds, colramp = rf, xlab = "Carat", ylab = "Price ($)")


#Mosiac Plots

data(HairEyeColor)
mosaicplot(HairEyeColor)

#Heat Map

heatmap(as.matrix(mtcars))
image(as.matrix(b[2:7]))

#Map Visulisations
install.packages("leaflet")
install.packages("magrittr")
library(leaflet)
library(magrittr)

m <- leaflet() %>%
  addTiles %>%
  addMarkers(lng=77.2310, lat=28.6560, popup="The delicious food of chandni chowk")
m

#3D Maps

# Using Rcmdr GUI
install.packages("Rcmdr")
library(Rcmdr)

#data(iris, package="datasets")
#scatter3d(Petal.Width~Petal.Length+Sepal.Length|Species, data=iris, fit="linear"
#residuals=TRUE, parallel=FALSE, bg="black", axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE)


# Using Lattice Package

library(lattice)
attach(iris)# 3D scatterplot by factor level
cloud(Sepal.Length~Sepal.Width*Petal.Length|Species, main="3D Scatterplot by Species")
xyplot(Sepal.Width ~ Sepal.Length, iris, groups = iris$Species, pch= 20)



