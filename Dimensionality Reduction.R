# Dimensionality Reduction

set.seed(1234)
datamatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(datamatrix)[, nrow(datamatrix):1])
heatmap(datamatrix)


library(dplyr)
hh <- hclust(dist(datamatrix))
names(hh)
hh$order
datamatrixorder <- datamatrix[hh$order, ]

#Plot Ordered Matrix

par(mfrow = c(1,1))
image(t(datamatrixorder[, nrow(datamatrixorder):1]))


#SVD
 svd1 <- svd(scale(datamatrixorder))
 svd1
 names(svd1)
 svd1$d
 svd1$u
 svd1$v