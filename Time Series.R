# Time Series

# Create our own time series data

#Create dataset
my.data <- round(sin((0:47 %% 12)+1)+runif(48) ,1) 
my.data

#Comvert to Time series data, ranging from 1/1/2009 to 31/12/2014
my.ts <- ts(my.data, start=c(2009, 1), end=c(2014, 12), frequency=12)
my.ts
plot(my.ts)

#We can role the data up... Convert to quarters
my.ts.qrts <- ts(my.ts, start = c(2009,1), end= c (2014, 12), frequency = 4)
my.ts.qrts

# Using Plot(window), to plot a subset of the TS
plot(window(my.ts, start= c(2014, 6), end = c(2014, 12)))

# A vectore of times at which time series were sampled
time(my.ts)

# Gives the position in the cycle of eahc observation
cycle(my.ts)

# Comparing this year vs last year
tsl <- lag(my.ts, k = 12)
plot(my.ts, lwd = 2, main = "Comparison with Next 12 Months")
points(tsl, col = "red", type = "l")

# Difference vector the time series, d times
ds <- diff(my.ts, d=1)     
plot( ds ) 

-------------------------------------------------------------------------------------------------------------------------------------

  # Import TUI Dataset
  tui <- read.csv2("~/Documents/R - WD/Datasets/tui.csv")

# Quick view of the dataset

head(tui)
summary(tui$close)

# Initial Plot of the dataset
plot(tui[,5], type = 'l', lwd = 2, col = "red", xlab = "Time", ylab = "Closing stock value", main = "Stock data for TUI", ylim = c(10, 60))

hist(diff(tui[,5]), prob = TRUE, col = "grey", breaks = 20, main = "Histogram of Differences", xlim = c(-4.5, 4.5), ylim = c(0,0.7))
lines(density(diff(tui[,5])), lwd = 2)
points(seq(-4.5,4.5,len=100),dnorm(seq(-4.5,4.5,len=100), mean(diff(tui[,5])), sd(diff(tui[,5]))), col='red', type='l', lwd = 2)

# The Kolgomorov-Smirnoff test checks is a sample – in this case the differences between consecutive values of the time series – follows a specific distribution

# We must first take the log difference between eacg TS observation
ds <- diff(log(tui[,5]))
ds
ks.test(ds, "pnorm", mean(ds), sd(ds))

# Checks to see if the sample is normally distributed
qqnorm (diff(tui[,5])); abline(0,1, col= "red") 

# Tt=∑i=−∞+∞λiXt+i

# Tt=12a+1∑−aaXt+i

library("TTR")

tui.ma1 <- SMA(tui[,5], 20)
tui.ma2 <- SMA(tui[,5], 50)

a <- 20
b <- 50
tui.ma3 <- filter(tui[,5], filter = rep(1/a,a))
tui.ma4 <- filter(tui[,5], filter = rep(1/b,b))
ts.plot(tui[,5], tui.ma3, tui.ma4, col=1:3, lwd=c(1,2,2))

ts.plot(tui[,5], tui.ma1, tui.ma2, col=1:3, lwd=c(1,2,2))

-------------------------------------------------------------------------------------------------------------------------------------

  
beer <- read.csv2("~/Documents/R - WD/Datasets/beer.csv", header=T, dec=",", sep=";")
beer <- ts(beer[,1],start=1956,freq=12) # Creates a Time Series which starts at the yeat 1956, with 12 months freq
plot(beer)
plot(stl((beer), s.window="periodic")) 
plot(stl(log(beer), s.window="periodic")) # Logging normalises the data

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Linear Smoothing

  
logbeer <- log(beer)
logbeer
plot(logbeer)
plot(beer)
t <- seq(1956, 1995.2, len = length(beer))
t  
t2 <- t^2 
t2
model <- lm(logbeer ~ t + t2)
model$fitted.values
plot(model$fit)
plot(logbeer)
lines(t, model$fit, col = 'red', lwd = 2) # However this does not consider the seasonal component of the time series 

# log(Xt)=α0+α1t+α2t2+βcos(2π12)+γsin(2π12)+et

cos.t <- cos(2*pi*t)
sin.t <- sin(2*pi*t)
model.trig <- lm(logbeer ~ t + t2 + sin.t + cos.t)

plot(logbeer)
lines(t, model.trig$fit, col = "red", lwd = 2)

summary(model.trig)

# Its seems that the cosine component doesnt have much of an effect, so i would remove it (however this is diffrent in each dataset)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
# Exponential Smoothing - Weights more recent events more than observations that are less recent
  #Exponential Smoothing doesnt work well when there's a trend in the data --> double exponential smoothing
  
model.holt <- HoltWinters(beer)
plot(beer)
lines(model.holt$fitted[,"xhat"], col = "red", lwd = 2)

# Now to predict the next value in the next 24 months of the times series

preds <- predict(model.holt, n.ahead = 24)
plot(beer, xlim = c(1956, 1997))
lines(preds, col = "red")


# An example with the TUI time series data

model.tui.seasonal <- HoltWinters(tui[,5])
model.tui.nonseasonal <- HoltWinters(tui[,5], gamma = FALSE)
plot(tui[,5], type = 'l', xlim = c(0, length(tui[,5])+36))
lines(model.tui.nonseasonal$fitted[,"xhat"], col = "red", lwd = 2)

#Predict the next 36 Months
preds.tui <- predict(model.tui.nonseasonal, n.ahead = 36)
lines(preds.tui, col = "green", type = "l")

# Using the forecast package

library(forecast)
plot(forecast(model.tui.nonseasonal, h=40, level=c(75,95))) # the next 40 data points using 75% and 95% confidence intervals

# Automated forecasting using an exponential model
fit <- ets(my.ts)
plot(forecast(fit, levels = c(75, 95)))




  
