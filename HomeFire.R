# Import datasets

homefire <- read.csv("~/Documents/R - Working Directory/R-Code/Homefire/HomeFire.csv")

View(homefire)

summary(homefire)
str(homefire)

homefire$id <- as.factor(homefire$id)
homefire$user_account <- as.factor(homefire$user_account)

boxplot(homefire$pos_x)
boxplot(homefire$pos_y)

# visualise dataset

plot(homefire$pos_x)
plot(homefire$pos_y)
plot(homefire$pos_y ~ homefire$pos_x)

# 3D-Scatter Plots
# install.packages("car")
library("car")

scatter3d(x=homefire$pos_x, z=homefire$pos_y, y=homefire$time)
scatter3d(time ~ pos_y * pos_x, data=homefire, groups=factor(homefire$event), fit = "smooth")
scatter3d(time ~ pos_y * pos_x, data=homefire, groups=factor(homefire$event), fit="smooth", surface=FALSE, ellipsoid=TRUE, grid=FALSE)

# install.packages("MASS")
require(MASS)
# Two-Dimensional Kernel Density Estimation
dens <- kde2d(homefire$pos_x, homefire$pos_y)  #overrode default bandwidth

#Visualisation 
filled.contour(dens, color.palette = terrain.colors)
filled.contour(dens, color.palette = cm.colors)

#K-means Clusters
homefire.clusters <- kmeans(homefire$pos_x, 3)
homefire.clusters
homefire.clusters$centers
plot(homefire[c("pos_x", "pos_y")], col = homefire.clusters$cluster)


# We write an if/else statement in R to bucket the users into groups and take actions accordingly

