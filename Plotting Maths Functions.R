#Plotting Math Funcitons
curve(x^3 - 3*x, -2, 2, main = "Intersecting Curves", xlab = "X-Values", ylab = "Functions", lty = 1)
abline(h = 0, v = 0, lty = 9, col = "gray")
curve(x^2-2, add = TRUE,col = "violet")
curve(x^3-3, add = TRUE,col = "green")
curve(x^1-2, add = TRUE,col = "blue")
curve(x^5-2, add = TRUE,col = "red")
curve(x^4-2, add = TRUE,col = "purple")


#Same Scale as previous graph
curve(sin, -2*pi, 2*pi, xname = "t")
curve(tan, xname = "t", add = NA,
      main = "curve(tan)  --> same x-scale as previous plot")

#Adding limits
plot(cos, -pi,  3*pi)
curve(cos, xlim = c(-pi, 2*pi), n = 1001, col = "blue", add = TRUE)

#Combining Functions
chippy <- function(x) sin(cos(x)*exp(-x/2))
curve(chippy, -8, 7, n = 2001)
plot (chippy, -8, -5)

#Adding ABline
plot(qlogis, main = "The Inverse Logit : qlogis()")
abline(h = 0, v = 0:2/2, lty = 3, col = "gray")
