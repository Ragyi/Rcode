# Setting up RSiteCatalyst

install.packages("devtools")
library(devtools)
install_github("randyzwitch/RSiteCatalyst", ref="master")
library(RSiteCatalyst)

install.packages(c("jsonlite","plyr","httr","stringr","digest","base64enc"))

library(RSiteCatalyst)
SCAuth("username", "shared secret")

reportsuites <- GetReportSuites()
View(reportsuites)

#FairFax Mastheads data 

#QueueOvertime Report - the only granularity allowed is time

date.from <- "2004-01-01"
date.to <- "2014-12-31"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews")
report.data.overtime <- QueueOvertime(reportsuite.id, date.from, date.to, metrics)
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

# QueueTrended - an ordered list of elements and associated metrics with time granularity.
date.from <- "2014-01-01"
date.to <- "2014-01-07"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews","event10")
elements <- c("page","geoCountry","geoCity")

report.data <- QueueTrended(reportsuite.id, date.from, date.to, metrics, elements)

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

#QueuePathing - an ordered list of paths matching the specified pattern.

ate.from <- "2014-01-01"
date.to <- "2014-01-07"
reportsuite.id <- "your_report_suite"
metric <- "pageviews"
element <- "page"
pattern <- c("Home","::anything::","::anything::")

report.data <- QueuePathing(reportsuite.id, date.from, date.to, metric, element, pattern)

#QueueFallout - a pathed list of elements, with fallout values for each.

date.from <- "2014-01-01"
date.to <- "2014-01-07"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews","event10")
element <- "page"
checkpoints <- c("Home","Contact","ThankYou")

report.data <- QueuePathing(reportsuite.id, date.from, date.to, metrics, element, checkpoints)

#Get Elements
elements.valid <- GetElements("your_report_suite",metrics=c('visitors','pageviews'),elements=c('page','geoCountry'),date.granularity='day')

#Get Metrics
metrics.valid <- GetMetrics("your_report_suite",metrics=c('visitors','pageviews'),elements=c('page','geoCountry'),date.granularity='day')

#Get Evars
evars <- GetEvars(c("your_prod_report_suite","your_dev_reportsuite"))

#Get Props
props <- GetProps(c("your_prod_report_suite","your_dev_reportsuite"))

#Get Successfull Events
successevents <- GetSuccessEvents(c("your_prod_report_suite","your_dev_reportsuite"))

#Get Segments
segments <- GetSegments(c("your_prod_report_suite","your_dev_reportsuite"))


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
#Data Exploration 
  
plot(report.data.overtime$uniquevisitors ~ report.data.overtime$pageviews)
fit.ffx <- lm(uniquevisitors ~ pageviews, data = report.data.overtime)
lines(report.data.overtime$pageviews, fitted(fit.ffx), col = "red")
abline(v= 5e+06, col = "blue") 
abline(v= 3.0e+07, col = "green")

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Outlier Detection 
  
data.numeric <- dataa[sapply(dataa,is.numeric)]
data.numeric <- data.numeric[,-1]
View(data.numeric)  

library(DMwR)
outlier.scores <- lofactor(data.numeric$visits, k=5)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:5]
print(outliers)

n <- nrow(data.numeric)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(data.numeric), cex=.8, xlabs=labels)

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(data.numeric, pch=pch, col=col)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(rgl)
library(car)

scatter3d(x=data.kmeans$visits, z=data.kmeans$pageviews, y=data.kmeans$uniquevisitors)
scatter3d(visits ~ pageviews * uniquevisitors, data=data.kmeans, fit="smooth")
scatter3d(visits ~ pageviews * uniquevisitors, data=data.kmeans, fit="smooth", surface=FALSE, ellipsoid=TRUE, grid=FALSE)


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# K-Means Clustering
data.kmeans <- data.numeric[ , -c(1:2)]
ffx.kmeans <- kmeans(data.kmeans, 5)
ffx.kmeans
ffx.kmeans$centers
ffx.kmeans$clusters
plot(data.kmeans[c("visits", "pageviews", "uniquevisitors")], col = ffx.kmeans$cluster)
