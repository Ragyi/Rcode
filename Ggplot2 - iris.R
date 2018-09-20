
attach(iris)
plot(Sepal.Length ~ Sepal.Width)

library("ggplot2")

qplot(Sepal.Width, Sepal.Length, data = iris, col == Species)

g <- ggplot(iris, aes(Sepal.Length, Sepal.Width))

g + geom_point(aes(col = Species), size = 4, alpha = 1/2)


