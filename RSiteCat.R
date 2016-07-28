
# Installing RSiteCatalyst

install.packages("devtools")
library(devtools)
install.packages("RSiteCatalyst", dependencies = TRUE)
install_github("randyzwitch/RSiteCatalyst", ref="master")
library(RSiteCatalyst)

install.packages(c("jsonlite","plyr","httr","stringr","digest","base64enc"))

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
Authorisation

SCAuth("username:accountName", "sharedSecret") # Change this according to the account you're using

reportsuites <- GetReportSuites()
View(reportsuites)
  
#QueueOvertime Report - the only granularity allowed is time
today_date <- Sys.Date()
yesterday_date <- today_date - 1
  
date.from <- yesterday_date
date.to <- today_date
reportsuite.id <- "your_report_suite"
metrics <- c("visits","pageviews", "event40",
             "event15", "event2", "event32", "event4")
report.data.overtime<- QueueOvertime(reportsuite.id, date.from, date.to, metrics)
View(report.data.overtime)

# Optinal Extras
#date.from <- "2014-01-01"
#date.to <- "2014-01-07"
#reportsuite.id <- "your_report_suite"
#metrics <- c("visits","uniquevisitors","pageviews")
#date.granularity <- "hour"
#segment.id <- "Visit_Natural_Search"
#anomaly.detection <- TRUE
#data.current <- TRUE
#expedite <- TRUE
#report.data <- QueueOvertime(reportsuite.id, date.from, date.to, metrics,date.granularity=date.granularity,segment.id=segment.id,anomaly.detection=anomaly.detection,data.current=data.current,expedite=expedite)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
#QueueRanked - an ordered list of elements and associated metrics with no time granularity.

date.from <- "2014-01-01"
date.to <- "2015-01-11"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews","event10")
report.data <- QueueRanked(reportsuite.id, date.from, date.to, metrics, elements)
View(report.data)

# Optional Extras
#date.from <- "2014-01-01"
#date.to <- "2013-01-07"
#reportsuite.id <- "your_report_suite"
#metrics <- c("visits","uniquevisitors","pageviews","event10","event10")
#elements <- c("page","geoCountry","geoCity")
#top <- 100
#start <- 100
#selected <- c("Home","Search","About")
#segment.id <- "dw:12345"
#data.current <- TRUE
#expedite <- TRUE
#report.data <- QueueRanked(reportsuite.id, date.from, date.to, metrics,elements,top=top,start=start,selected=selected,segment.id=segment.id,data.current=data.current,expedit=expedite)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
# QueueTrended - an ordered list of elements and associated metrics with time granularity.
date.from <- "2016-02-03"
date.to <- "2016-03-8"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","pageviews", "event40",
             "event15", "event2", "event32", "event4")
elements <- c("page","geoCountry","geoCity")
report.data.trended <- QueueTrended(reportsuite.id, date.from, date.to, metrics, elements)
View(report.data.trended)

# Optional Extras
#date.from <- "2014-01-01"
#date.to <- "2013-01-07"
#reportsuite.id <- "your_report_suite"
#metrics <- c("visits","uniquevisitors","pageviews","event10")
#elements <- c("page","geoCountry","geoCity")
#top <- 100
#start <- 100
#selected <- list(page=c("Home","Search","About"))
#date.granularity <- "hour"
#segment.id <- "dw:12345"
#data.current <- TRUE
#expedite <- TRUE
#report.data <- QueueTrended(reportsuite.id, date.from, date.to, metrics,elements,top=top,start=start,selected=selected,segment.id=segment.id,data.current=data.current,expedit=expedite)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
#QueuePathing - an ordered list of paths matching the specified pattern.

date.from <- "2014-01-01"
date.to <- "2014-01-07"
reportsuite.id <- "your_report_suite"
metric <- "pageviews"
element <- "page"
pattern <- c("Home","::anything::","::anything::")

report.data <- QueuePathing(reportsuite.id, date.from, date.to, metric, element, pattern)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
#QueueFallout - a pathed list of elements, with fallout values for each.

date.from <- "2014-01-01"
date.to <- "2014-01-07"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews","event10")
element <- "page"
checkpoints <- c("Home","Contact","ThankYou")

report.data <- QueuePathing(reportsuite.id, date.from, date.to, metrics, element, checkpoints)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
#Get Elements
elements.valid <- GetElements("your_report_suite",metrics=c('visitors','pageviews'),elements=c('page','geoCountry'),date.granularity='day')

#Get Metrics
metrics.valid <- GetMetrics("your_report_suite",metrics=c('visitors','pageviews'),elements=c('page','geoCountry'),date.granularity='day')

#Get Evars
evars <- GetEvars(c("your_report_suite","your_report_suite"))

#Get Props
props <- GetProps(c("your_report_suite","your_report_suite"))

#Get Successfull Events
successevents <- GetSuccessEvents(c("your_report_suite","your_report_suite"))

#Get Segments
segments <- GetSegments(c("your_report_suite","your_report_suite"))


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  

#Data Exploration 

plot(report.data.overtime$event40 ~ report.data.overtime$pageviews)
fit.ffx <- lm(event40 ~ pageviews, data = report.data.overtime)
lines(report.data.overtime$pageviews, fitted(fit.ffx), col = "red")

# abline(v= 10000, col = "blue") 
# abline(v= 40000, col = "green")
# ("visits","pageviews", "event40",
# "event15", "event2", "event32", "event4")

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Outlier Detection 

# Overtime Report
data.numeric.overtime <- report.data.overtime[sapply(report.data.overtime, is.numeric)]
data.numeric.overtime <- data.numeric.overtime[ ,-c(1:3)]
View(data.numeric.overtime) 

# Trended Report

data.numeric.trended <- report.data.trended[sapply(report.data.trended, is.numeric)]
data.numeric.trended <- data.numeric.trended[ ,-c(1:3)]
data.numeric.trended <- data.numeric.trended[!is.na(data.numeric.trended), ]
View(data.numeric.trended) 

summary(data.numeric.trended)

# Overtime Report

# install.packages("DMwR")
library(DMwR)
outlier.scores <- lofactor(data.numeric.overtime$visits, k=3)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:5]
print(outliers)

data.numeric[c(25,29,16,3,24), ]

n <- nrow(data.numeric.overtime)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(data.numeric.overtime), cex=.8, xlabs=labels)

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(data.numeric.overtime, pch=pch, col=col)

# Trended Report

library(DMwR)
outlier.scores <- lofactor(data.numeric.trended$visits, k=3)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:5]
print(outliers)

data.numeric.trended[c(25,29,16,3,24), ]

n <- nrow(data.numeric.trended)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(data.numeric.trended), cex=.8, xlabs=labels)

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(data.numeric.trended, pch=pch, col=col)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
# K-Means Clustering
data.kmeans <- data.numeric
ffx.kmeans <- kmeans(data.kmeans, 3)
ffx.kmeans
ffx.kmeans$centers
ffx.kmeans$clusters
plot(data.kmeans[c("visits","pageviews", "event40",
                   "event15", "event2", "event32", "event4")], col = ffx.kmeans$cluster)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(rgl)
library(car)
library(rattle)

scatter3d(x=data.kmeans$pageviews, z=data.kmeans$event15, y=data.kmeans$event40)
scatter3d(event40 ~ pageviews * event15, data=data.kmeans, fit="smooth")
scatter3d(event40 ~ pageviews * event15, data=data.kmeans, fit="smooth", surface=FALSE, ellipsoid=TRUE, grid=FALSE)


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Shiny App
  
library(shiny)
runApp('hcf')

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Decision Tree Model  
  
library(rpart)
library(rpart.plot)
library(RColorBrewer)

# Model 1

fit1 <- rpart(event40 ~ pageviews + visits +
              event15 + event2 + event32 + event4, 
              data = report.data.overtime, 
              method = 'anova', 
              control = rpart.control(minsplit = 2, cp = 0.01)) 
# Cp = Complexity Parameter - 
# Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.

fancyRpartPlot(fit1)
text(fit1)

# Model 2

fit2 <- rpart(event40 ~ pageviews + visits +
              event15 + event2 + event32 + event4, 
              data = report.data.overtime, 
              method = 'poisson', 
              control = rpart.control(minsplit = 2, cp = 0.01))
fancyRpartPlot(fit2)



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Caret Models
  
library(ggplot2)
library(lattice)
library(caret)

set.seed(1998)
inTraining <- createDataPartition(data.numeric$event40, p = .75, list = FALSE)
training <- data.numeric[ inTraining,]
testing  <- data.numeric[-inTraining,]

fitControl1 <- trainControl(method = "oob",
                           classProbs = FALSE,
                           search = "random")
fitControl2 <- trainControl(
  ## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# Artificial Neural Nets

library(nnet)
nnetGrid <- expand.grid(.decay = c(0.01, 0.02), 
                        .size = c(5, 6, 7))
nnetfit <- train(event40 ~ ., 
                data = training,
                method = "nnet", 
                maxit = 1000, 
                tuneGrid = nnetGrid,
                trace = F, 
                linout = 1) 
print(nnetfit)

nnetPredict <- predict(nnetfit, 
                            newdata = testing)
nnetPredict

nnetRmse <- sqrt(mean((nnetPredict - testing$event40)^2)) 
nnetRmse

# Random Forest Model (Caret)

set.seed(1895)
rf_model<-train(event40 ~ .,
                data = training,
                method="cforest",
                trControl = fitControl2, 
                control = cforest_unbiased(ntree = 50))
print(rf_model)

cforestPredict <- predict(rf_model, 
                       newdata = testing)
print(cforestPredict)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Random Forest Model

library(randomForest)

set.seed(415)
random.fit <- randomForest(event40 ~ ., 
                           data = training, 
                           importance = TRUE, 
                           ntree = 5000)

varImpPlot(random.fit, col = 'blue')

plot.randomForest(random.fit)
summary(random.fit)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # End!
